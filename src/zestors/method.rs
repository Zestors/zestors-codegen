use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{
    bracketed, parenthesized,
    parse::Parse,
    parse2, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Colon, Comma},
    Expr, FnArg, Generics, ImplItem, ImplItemMethod, ItemImpl, PatType, Receiver, ReturnType,
    Token, Type, Visibility, WherePredicate,
};

use super::attrs::{AsAttr, ZestorsAttrs, ZestorsMethodAttrs};

//------------------------------------------------------------------------------------------------
//  ParsedActorMethodArgs
//------------------------------------------------------------------------------------------------

/// The arguments to an actor function, parsed to make handling easier.
pub struct ZestorsMethodArgs {
    pub actor_arg: Receiver,
    pub msg_arg: PatType,
    pub snd_arg: Option<PatType>,
    pub state_arg: Option<PatType>,
}

impl ZestorsMethodArgs {
    /// ## Returns
    /// ```ignore
    /// Rcv<String>
    /// // or
    /// ()
    /// ```
    pub fn rcv_ty(&self) -> Type {
        match &self.snd_arg {
            Some(snd) => {
                let snd_arg = match &*snd.ty {
                    Type::Path(path) => match &path.path.segments.last().unwrap().arguments {
                        syn::PathArguments::AngleBracketed(args) => args.args.first().unwrap(),
                        _ => panic!("Should be Snd<R>"),
                    },
                    _ => panic!("Should be Snd<R>"),
                };
                parse_quote!( zestors::core::Rcv<#snd_arg> )
            }
            None => parse_quote!(()),
        }
    }

    /// ## Returns
    /// ```ignore
    /// P, u32, ...
    /// ```
    pub fn param_t_types(&self) -> Punctuated<Type, Comma> {
        let mut addr_args = Punctuated::<Type, Comma>::new();

        match &*self.msg_arg.ty {
            Type::Tuple(msg_element_types) => match msg_element_types.elems.len() {
                0 => addr_args.push(parse_quote!{ () }),
                _ => msg_element_types.elems.iter().for_each(|msg_element_ty| {
                    addr_args.push(msg_element_ty.clone());
                }),
            },
            _ => addr_args.push(*self.msg_arg.ty.clone()),
        };

        addr_args
    }

    /// ## Returns
    /// ```ignore
    /// P: ParamType<'z, AT>, u32: ParamType<'z, AT>
    /// ```
    pub fn params_impl_predicates(&self) -> Punctuated<WherePredicate, Comma> {
        let mut predicates: Punctuated<WherePredicate, Comma> = Punctuated::new();

        match &*self.msg_arg.ty {
            Type::Tuple(msg_element_types) => {
                match msg_element_types.elems.len() {
                    0 => predicates.push(parse_quote!{ (): zestors::core::ParamType<'z, AT> }),
                    _ => msg_element_types.elems.iter().for_each(|msg_element_ty| {
                        predicates.push(parse_quote! {
                            #msg_element_ty: zestors::core::ParamType<'z, AT>
                        });
                    }),
                }
            }
            _ => {
                let arg_ty = &self.msg_arg.ty;
                predicates.push(parse_quote! {
                    #arg_ty: zestors::core::ParamType<'z, AT>
                })
            }
        };

        predicates
    }

    /// ## Returns
    /// ```ignore
    /// param1: Param<'z, P, AT>, param2: Param<'z, u32, AT>, ...
    /// ```
    pub fn param_args(&self) -> Punctuated<FnArg, Comma> {
        let mut addr_args = Punctuated::<FnArg, Comma>::new();

        match &*self.msg_arg.ty {
            Type::Tuple(msg_element_types) => match msg_element_types.elems.len() {
                0 => {
                    let arg_pat = &self.msg_arg.pat;
                    let arg_ty = &self.msg_arg.ty;
                    addr_args.push({
                        parse_quote! { #arg_pat: zestors::core::Param<'z, #arg_ty, AT> }
                    });
                    // panic!("{}", quote::quote! { #arg_pat: zestors::core::Param<'z, #arg_ty, AT> }.to_token_stream().to_string())
                }
                _ => {
                    let msg_element_idents = match &*self.msg_arg.pat {
                        syn::Pat::Tuple(idents) => idents,
                        _ => panic!("Invalid syntax"),
                    };

                    msg_element_types
                            .elems
                            .iter()
                            .zip(msg_element_idents.elems.iter())
                            .for_each(|(msg_element_ty, msg_element_ident)| {
                                addr_args.push(parse_quote!{ #msg_element_ident:  zestors::core::Param<'z, #msg_element_ty, AT> });
                            })
                }
            },
            _ => {
                let arg_ty = &self.msg_arg.ty;
                let arg_pat = &self.msg_arg.pat;

                addr_args.push({
                    parse_quote! { #arg_pat: zestors::core::Param<'z, #arg_ty, AT> }
                })
            }
        };

        addr_args
    }

    pub fn new(mut actor_inputs: Punctuated<FnArg, Comma>) -> Self {
        // All arguments that can possibly be created from an actor method.
        let actor_arg: FnArg;
        let msg_arg: FnArg;
        let mut snd_arg: Option<FnArg> = None;
        let mut state_arg: Option<FnArg> = None;

        // match the actor inputs, and then parse the correct arguments based on their
        // positions.
        match actor_inputs.len() {
            2 => {
                // (&mut A, M)
                msg_arg = actor_inputs.pop().unwrap().into_value();
                actor_arg = actor_inputs.pop().unwrap().into_value();
            }
            3 => {
                // (&mut A, M, Snd<R>) or (&mut A, M, &mut State<A>)
                let second = actor_inputs.iter().nth(2).unwrap().clone();
                if let FnArg::Typed(typed_arg) = second {
                    if let Type::Reference(_) = &*typed_arg.ty {
                        // (&mut A, M, &mut State<A>)
                        state_arg = Some(actor_inputs.pop().unwrap().into_value());
                        msg_arg = actor_inputs.pop().unwrap().into_value();
                        actor_arg = actor_inputs.pop().unwrap().into_value();
                    } else {
                        // (&mut A, M, Snd<R>)
                        snd_arg = Some(actor_inputs.pop().unwrap().into_value());
                        msg_arg = actor_inputs.pop().unwrap().into_value();
                        actor_arg = actor_inputs.pop().unwrap().into_value();
                    }
                } else {
                    panic!("Not allowed here!")
                }
            }
            4 => {
                // (&mut A, M, Snd<R>, &mut State<A>)
                state_arg = Some(actor_inputs.pop().unwrap().into_value());
                snd_arg = Some(actor_inputs.pop().unwrap().into_value());
                msg_arg = actor_inputs.pop().unwrap().into_value();
                actor_arg = actor_inputs.pop().unwrap().into_value();
            }
            _ => {
                panic!("Incorrect input arguments for handler function")
            }
        };

        // And finally return the self
        Self {
            actor_arg: match actor_arg {
                FnArg::Receiver(receiver) => receiver,
                FnArg::Typed(_) => panic!("not allowed here"),
            },
            msg_arg: match msg_arg {
                FnArg::Receiver(_) => panic!("not allowed here"),
                FnArg::Typed(pat) => pat,
            },
            snd_arg: snd_arg.map(|snd| match snd {
                FnArg::Receiver(_) => panic!("not allowed here"),
                FnArg::Typed(pat) => pat,
            }),
            state_arg: state_arg.map(|state| match state {
                FnArg::Receiver(_) => panic!("not allowed here"),
                FnArg::Typed(pat) => pat,
            }),
            // msg_elements: Punctuated::new(),
        }
    }
}

//------------------------------------------------------------------------------------------------
//  ZestorsMethod
//------------------------------------------------------------------------------------------------

/// The combination of all attributes that a method can have.
pub struct ZestorsMethod<'a> {
    pub attrs: ZestorsMethodAttrs,
    pub args: ZestorsMethodArgs,
    pub org_method: &'a mut ImplItemMethod,
}

impl<'a> ZestorsMethod<'a> {
    pub fn vis(&'a self) -> &'a Visibility {
        match &self.attrs.As {
            Some(AsAttr(_As, vis, _ident)) => vis,
            None => &self.org_method.vis,
        }
    }

    pub fn ident(&'a self) -> Ident {
        match &self.attrs.As {
            Some(AsAttr(_As, _vis, ident)) => ident.clone(),
            None => {
                let handle_fn = self.org_method.sig.ident.to_string();

                let addr_fn = handle_fn
                    .strip_prefix("handle_")
                    .expect("Functions should start with 'handle_'");

                Ident::new(addr_fn, self.org_method.sig.ident.span())
            }
        }
    }

    pub fn take_attrs_from_method(item: &'a mut ImplItem) -> syn::Result<Option<Self>> {
        if let ImplItem::Method(method) = item {
            let attrs = ZestorsMethodAttrs::take_attrs_from(&mut method.attrs)?;
            Ok(Some(Self {
                args: ZestorsMethodArgs::new(method.sig.inputs.clone()),
                attrs,
                org_method: method,
            }))
        } else {
            Ok(None)
        }
    }
}
