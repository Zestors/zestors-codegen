pub mod attrs;
pub mod method;

use proc_macro2::{Ident, Punct, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    braced, bracketed, custom_keyword, parenthesized,
    parse::Parse,
    parse2, parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::{Colon, Comma, For, Impl, Token, Where},
    Attribute, AttributeArgs, Block, Expr, ExprParen, FnArg, Generics, ImplItem, ImplItemMethod,
    ItemImpl, ItemStruct, Pat, PatType, Path, Receiver, ReturnType, Signature, Token, Type,
    TypeGenerics, TypeParen, TypePath, Visibility, WhereClause, spanned::Spanned,
};

use crate::derive_addr::{addr_generics, addr_ident_from_actor_ident};

use self::{
    attrs::ZestorsAttrs,
    method::{ZestorsMethod, ZestorsMethodArgs},
};

pub fn zestors_proc_macro_attr(
    impl_attributes: ZestorsAttrs,
    mut actor_impl: ItemImpl,
) -> syn::Result<TokenStream> {
    // Traits cannot be implemented on the actor, but should be implemented on the address
    if let Some(trait_impl) = &actor_impl.trait_ {
        Err(
            syn::Error::new(trait_impl.1.span(), 
            "Traits should be implemented on the address instead of the Actor.\n\n#[zestors(impl Trait for Addr)]")
        )?;
    }

    let zestors = Zestors::take_attrs_from_methods(&mut actor_impl, impl_attributes)?;

    let actor_ty = &zestors.actor_ty;

    let addr_methods = zestors
        .methods
        .iter()
        .filter(|method| method.attrs.Ignore.is_none())
        .map(|handler_method| {
            //-------------------------------------------------
            //  Take out the main parts of the handler method
            //-------------------------------------------------
            let handler_method_args: &ZestorsMethodArgs = &handler_method.args;
            let original_actor_method: &ImplItemMethod = &*handler_method.org_method;

            //-------------------------------------------------
            //  Parse everything related to the parameters
            //-------------------------------------------------

            // P, u32, ...
            let method_param_t_types = handler_method_args.param_t_types();

            // param1: Param<'z, P, AT>, param2: Param<'z, u32, AT>, ...
            let method_param_args = handler_method_args.param_args();

            // param1, param2, ...
            let method_param_pats = addr_pats(&method_param_args);

            // Param<'z, P, AT>, param2: Param<'z, u32, AT>
            let method_param_types = addr_types(&method_param_args);

            // P: ParamType<'z, AT>, u32: ParamType<'z, AT>,
            let method_params_impl_predicates = handler_method_args.params_impl_predicates();




            //-------------------------------------------------
            //  Parse everything related to the original actor method
            //-------------------------------------------------

            // handle_echo
            let actor_method_ident = &original_actor_method.sig.ident;
            //  (P: Send,              P,                  where P: 'static   )
            let (method_impl_generics, method_ty_generics, method_where_clause) =
                &original_actor_method.sig.generics.split_for_impl();
            // ::<P> or nothing
            let optional_method_ty_generics = {
                let generics = parse2::<Generics>(method_ty_generics.into_token_stream()).unwrap();
                match generics.lt_token.is_some() {
                    true => quote! { ::#generics },
                    false => quote! {},
                }
            };
            let original_doc_attrs = original_actor_method
                .attrs
                .iter()
                .filter(|attr| {
                    if let Ok(meta) = attr.parse_meta() {
                        if let syn::Meta::NameValue(val) = meta {
                            if val.path.to_token_stream().to_string() == "doc" {
                                return true;
                            }
                        }
                    }
                    false
                })
                .collect::<Vec<_>>();

            // panic!("{}", original_doc_attrs.len());

            // P: 'static
            let method_where_clause_args =
                method_where_clause.map(|where_clause| &where_clause.predicates);

            //-------------------------------------------------
            //  Parse everyting related to the method
            //-------------------------------------------------

            let method_ident = handler_method.ident(); // echo
            let method_vis = handler_method.vis(); // pub | pub(crate)
            let method_rcv = handler_method_args.rcv_ty(); // Rcv<String> | ()

            let doc = format!(
                    "_Automatically generated function for `{actor}`._\n## Signature\n```ignore \n {vis} fn {method}({args}) -> {rcv_ty} {where_clause}; \n```\n # Docs",
                actor = {
                    let mut val = actor_ty.to_token_stream().to_string();
                    val.retain(|s| !s.is_whitespace());
                    val
                },
                args = match method_param_pats
                    .iter()
                    .zip(&method_param_t_types)
                    .map(|(pat, ty)| {
                        format!(
                            "{}: {}",
                            pat.to_token_stream().to_string(),
                            ty.to_token_stream().to_string()
                        )
                    })
                    .reduce(|acc, new| format!("{}, {}", acc, new)) {
                        Some(str) => str,
                        None => "_: ()".to_string(),
                    },
                rcv_ty = {
                    let mut val = match &method_rcv {
                        Type::Path(path) => path.path.segments.last().unwrap().to_token_stream().to_string(),
                        Type::Tuple(_) => "()".to_string(),
                        _ => unreachable!(), 
                    };
                    val.retain(|s| !s.is_whitespace());
                    val
                },
                method = {
                    let mut val = quote! { #method_ident #method_impl_generics }.to_string();
                    val.retain(|s| !s.is_whitespace());
                    val
                },
                vis = {
                    let mut val = method_vis.to_token_stream().to_string();
                    val.retain(|s| !s.is_whitespace());
                    val
                },
                where_clause = 
                    match method_where_clause_args {
                        Some(args) => format!("where {}", {
                            let mut val = args.to_token_stream().to_string();
                            val.retain(|s| !s.is_whitespace());
                            val
                        }),
                        None => "".to_string(),
                    }
                    
            );

            //-------------------------------------------------
            //  And finally create the method
            //-------------------------------------------------

            // -- Example of a full method that is generated --
            //
            // pub(crate) fn echo<P: 'static>(
            //     &self,
            //     message1: Param<'z, P, AT>,
            //     message2: Param<'z, u32, AT>,
            // ) -> AT::CallResult<(P, u32), Rcv<String>>
            // where
            //     Param<'z, P, AT>,
            //     Param<'z, u32, AT>
            //     (Param<'z, P, AT>, Param<'z, u32, AT>): Into<AT::Msg<(P, u32)>>,
            //     P: Send
            // {
            //     self.call(Fn!(MyActor::<T>::handle_echo<P>), (message1, message2))
            // }

            quote! {
                #[doc = #doc]
                #(#original_doc_attrs)*
                #method_vis fn #method_ident #method_impl_generics // pub fn echo<P>
                    (
                        &self,
                        #method_param_args // param1: Param<'z, String, AT>, param2: Param<'z, u32, AT>, ...
                    )
                    -> AT::CallResult<(#method_param_t_types), #method_rcv> // -> CallResult<AT, (P, u32), Rcv<String>>
                where
                    #method_params_impl_predicates, // P: ParamType<'z, AT>, u32: ParamType<'z, AT>
                    (
                        #method_param_types // Param<'z, P, AT>, Param<'z, u32, AT>
                    ): Into<AT::Msg<(
                        #method_param_t_types // P, u32
                    )>>,
                    #method_where_clause_args // P: 'static
                {
                    <Self as zestors::core::Addressable<_>>::call(
                        self,
                        zestors::Fn!(
                            // MyActor  ::<T>               :: handle_echo       :: <P>
                            <#actor_ty>::#actor_method_ident #optional_method_ty_generics
                        ),
                        (
                            #method_param_pats // param1, param2
                        )
                    )
                }
            }
        })
        .collect::<Vec<_>>();


    let addr_type = zestors.addr_type();
    let addr_generics = zestors.addr_generics_with_lifetime();
    let (addr_impl_generics, addr_ty_generics, addr_where_clause) = &addr_generics.split_for_impl();

    Ok(quote!(
        #actor_impl

        impl #addr_impl_generics #addr_type #addr_where_clause {
            #(#addr_methods)*
        }
    ))
}

// fn

fn addr_pats(addr_args: &Punctuated<FnArg, Comma>) -> Punctuated<&Pat, Comma> {
    addr_args
        .iter()
        .map(|arg| {
            if let FnArg::Typed(arg) = arg {
                &*arg.pat
            } else {
                unreachable!()
            }
        })
        .collect()
}

fn addr_types(addr_args: &Punctuated<FnArg, Comma>) -> Punctuated<&Type, Comma> {
    addr_args
        .iter()
        .map(|arg| {
            if let FnArg::Typed(arg) = arg {
                &*arg.ty
            } else {
                unreachable!()
            }
        })
        .collect()
}

//------------------------------------------------------------------------------------------------
//  Zestors
//------------------------------------------------------------------------------------------------

pub struct Zestors<'a> {
    pub methods: Vec<ZestorsMethod<'a>>,
    pub attrs: ZestorsAttrs,
    pub actor_generics: &'a Generics,
    pub actor_ty: &'a Type,
}

impl<'a> Zestors<'a> {
    /// Removes all zestors method-attributes from the item, and parses and collects them into
    /// a `Vec<ZestorsMethod>`
    pub fn take_attrs_from_methods(item: &'a mut ItemImpl, attrs: ZestorsAttrs) -> syn::Result<Self> {
        let methods = item
            .items
            .iter_mut()
            .filter_map(|item| match ZestorsMethod::take_attrs_from_method(item) {
                Ok(Some(item)) => Some(Ok(item)),
                Ok(None) => None,
                Err(e) => Some(Err(e)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            methods,
            attrs,
            actor_generics: &item.generics,
            actor_ty: &*item.self_ty,
        })
    }

    pub fn addr_generics_with_lifetime(&self) -> Generics {
        let mut addr_generics = addr_generics(&self.actor_generics);
        addr_generics.params.push(parse_quote!( 'z ));
        addr_generics
    }

    pub fn addr_type(&self) -> Type {
        let mut addr_type = self.actor_ty.clone();

        match &mut addr_type {
            Type::Path(path) => {
                let last_segment = path.path.segments.last_mut().unwrap();
                last_segment.ident = match (&self.attrs.impl_expr, &self.attrs.for_expr) {
                    (Some((_impl, ident)), None) => ident.clone(),
                    (Some(_), Some((_for, ident))) => ident.clone(),
                    _ => addr_ident_from_actor_ident(&last_segment.ident),
                };

                match &mut last_segment.arguments {
                    syn::PathArguments::None => {
                        last_segment.arguments =
                            syn::PathArguments::AngleBracketed(parse_quote!(<AT>))
                    }
                    syn::PathArguments::AngleBracketed(arguments) => {
                        arguments.args.push(parse_quote!(AT))
                    }
                    syn::PathArguments::Parenthesized(_) => unreachable!(),
                }
            }
            _ => panic!("Not supported Self type!"),
        }

        addr_type
    }
}