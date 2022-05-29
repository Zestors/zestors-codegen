use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parse,
    parse2, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Brace, Colon, Gt, Lt, Semi, Struct},
    Attribute, Field, FieldsNamed, Generics, ItemStruct, Meta, NestedMeta, Token, Visibility,
};

use crate::kw;

//------------------------------------------------------------------------------------------------
//  #[derive(Addr)]
//------------------------------------------------------------------------------------------------

/// The #[derive(Addr)] macro
pub fn derive_addr(actor: ItemStruct) -> syn::Result<TokenStream> {
    let addr_struct = addr_struct(&actor)?;
    let addr_traits = addr_traits(&addr_struct, &actor);

    Ok(quote! {
        #addr_struct
        #addr_traits
    })
}

//------------------------------------------------------------------------------------------------
//  generate struct Addr
//------------------------------------------------------------------------------------------------

/// Create the addr struct
pub fn addr_struct(actor: &ItemStruct) -> syn::Result<ItemStruct> {
    let addr_attr = parse_addr_attr(&actor)?;

    let (ident, vis) = match addr_attr {
        Some(AddrAttr(_addr_kw, vis, ident)) => (ident, vis),
        None => (addr_ident_from_actor_ident(&actor.ident), actor.vis.clone()),
    };

    // panic!("{} {}", vis.to_token_stream().to_string(), ident.to_token_stream().to_string());

    Ok(ItemStruct {
        attrs: addr_docs(actor),
        vis,
        struct_token: token(Struct),
        ident,
        generics: addr_generics(&actor.generics),
        fields: addr_fields(&actor.ident, &actor.generics).into(),
        semi_token: Some(token(Semi)),
    })
}

fn addr_docs(actor: &ItemStruct) -> Vec<Attribute> {
    let original_docs = actor.attrs.iter().filter_map(|attr| {
        if let Ok(meta) = attr.parse_meta() {
            if let syn::Meta::NameValue(val) = meta {
                if val.path.is_ident("doc") {
                    return Some(attr.clone());
                }
            }
        }
        None
    });

    let actor_name = actor.ident.to_string();

    let mut docs = vec![
        parse_quote! {#[doc = "_Automatically generated address for `"]},
        parse_quote! {#[doc = #actor_name]},
        parse_quote! {#[doc = "`._"]},
        parse_quote! {#[doc = ""]},
        parse_quote! {#[doc = "# Docs"]},
    ];

    docs.extend(original_docs);

    docs
}

/// Generate the generics for an address from the actor generics
pub fn addr_generics(actor_generics: &Generics) -> Generics {
    let mut addr_generics = actor_generics.clone();

    addr_generics.lt_token = Some(token(Lt));
    addr_generics.gt_token = Some(token(Gt));
    addr_generics
        .params
        .push(parse_quote! { AT: zestors::core::AddrType = zestors::core::Local });

    addr_generics
}

pub fn addr_ident_from_attrs(actor: &ItemStruct) -> syn::Result<Option<Ident>> {
    Ok(parse_addr_attr(&actor)?.map(|val| val.2))
}

/// Generate the address identifier from the actor identifier
pub fn addr_ident_from_actor_ident(actor_ident: &Ident) -> Ident {
    format_ident!("{}Addr", actor_ident)
}

/// Generate the fields for the address
pub fn addr_fields(actor_ident: &Ident, actor_generics: &Generics) -> FieldsNamed {
    let mut address_fields = FieldsNamed {
        brace_token: token(Brace),
        named: Punctuated::new(),
    };

    address_fields.named.push(Field {
        attrs: Vec::new(),
        vis: syn::Visibility::Inherited,
        ident: Some(format_ident!("addr_type")),
        colon_token: Some(token(Colon)),
        ty: parse_quote! { std::marker::PhantomData<AT> },
    });

    let (_impl_generics, ty_generics, _where_clause) = actor_generics.split_for_impl();

    address_fields.named.push(Field {
        attrs: Vec::new(),
        vis: syn::Visibility::Inherited,
        ident: Some(format_ident!("addr")),
        colon_token: Some(token(Colon)),
        ty: parse_quote! { AT::Addr<#actor_ident #ty_generics> },
    });

    address_fields
}

//------------------------------------------------------------------------------------------------
//  impl Trait for Addr
//------------------------------------------------------------------------------------------------

/// Create all traits: Debug, Clone, Addressable, Address
fn addr_traits(addr: &ItemStruct, actor: &ItemStruct) -> TokenStream {
    let (actor_impl_generics, actor_ty_generics, actor_where_clause) =
        actor.generics.split_for_impl();
    let actor_ident = &actor.ident;

    let (addr_impl_generics, addr_ty_generics, addr_where_clause) = addr.generics.split_for_impl();
    let addr_ident = &addr.ident;
    let addr_ident_str = addr_ident.to_string();

    quote! {
        // impl ActorFor
        impl #actor_impl_generics zestors::core::ActorFor for #actor_ident #actor_ty_generics #actor_where_clause {
            type Addr<AT: zestors::core::AddrType> = #addr_ident #addr_ty_generics;
        }

        // implement debug
        impl #addr_impl_generics std::fmt::Debug for #addr_ident #addr_ty_generics #addr_where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#addr_ident_str)
                    .field("addr", &self.addr)
                    .finish()
            }
        }

        // unsafe impl #addr_impl_generics Send for #addr_ident #addr_ty_generics #addr_where_clause {}

        // implement Clone
        impl #addr_impl_generics Clone for #addr_ident #addr_ty_generics #addr_where_clause {
            fn clone(&self) -> Self {
                Self {
                    addr_type: self.addr_type.clone(),
                    addr: self.addr.clone(),
                }
            }
        }

        // implement Addressable
        impl #addr_impl_generics zestors::core::Addressable<#actor_ident #actor_ty_generics> for #addr_ident #addr_ty_generics #addr_where_clause {
            // type Actor = #actor_ident #actor_ty_generics;

            fn from_addr(addr: <Self::AddrType as zestors::core::AddrType>::Addr<#actor_ident #actor_ty_generics>) -> Self {
                Self {
                    addr_type: std::marker::PhantomData,
                    addr,
                }
            }

            fn as_addr(&self) -> &<Self::AddrType as zestors::core::AddrType>::Addr<#actor_ident #actor_ty_generics> {
                &self.addr
            }
        }

        // implement Address
        impl #addr_impl_generics zestors::core::Address for #addr_ident #addr_ty_generics #addr_where_clause {
            type AddrType = AT;

            fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> {
                self
            }

            fn as_any(&self) -> &dyn std::any::Any {
                self
            }

            fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
                self
            }

        }
    }
}

//------------------------------------------------------------------------------------------------
//  Address Attribute
//------------------------------------------------------------------------------------------------

/// Parses the `addr` attribute, to create a name.
fn parse_addr_attr(actor: &ItemStruct) -> syn::Result<Option<AddrAttr>> {
    let attrs = actor
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("addr"))
        .collect::<Vec<_>>();

    match attrs.len() {
        0 => Ok(None),
        1 => {
            let attr = attrs.first().unwrap();
            let addr_attr = parse2::<AddrAttr>(attr.to_token_stream())?;
            Ok(Some(addr_attr))
        }
        _ => Err(addr_error(
            "Only one #[addr] attribute is allowed.",
            attrs.first().unwrap().span(),
        ))?,
    }
}

pub struct AddrAttr(pub kw::addr, pub Visibility, pub Ident);
impl Parse for AddrAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![#]>()?;
        let bracketed;
        syn::bracketed!(bracketed in input);
        let addr = bracketed.parse::<kw::addr>()?;
        let parenthesized;
        syn::parenthesized!(parenthesized in bracketed);
        let vis = parenthesized.parse::<Visibility>()?;
        let ident = parenthesized.parse::<Ident>()?;
        Ok(Self(addr, vis, ident))
    }
}

//------------------------------------------------------------------------------------------------
//  Helper Functions
//------------------------------------------------------------------------------------------------

/// Helper function for creating tokens
fn token<T>(token: fn(Span) -> T) -> T {
    token(Span::call_site())
}

fn addr_error(str: &'static str, span: Span) -> syn::Error {
    syn::Error::new(
        span,
        format!(
            "{}\n\n-- Usage -- \n\n#[derive(Addr)]\n#[addr(MyAddrName)]\nstruct MyActor {{ ... }}",
            str
        ),
    )
}
