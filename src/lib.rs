#![allow(non_snake_case)]

pub(crate) mod derive_actor;
pub(crate) mod derive_addr;
pub(crate) mod derive_scheduler;
pub(crate) mod zestors;

use derive_addr::{addr_generics, addr_ident_from_actor_ident, addr_struct};
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    braced, bracketed, custom_keyword, parenthesized,
    parse::Parse,
    parse2, parse_macro_input, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Colon, Comma, For, Impl, Token, Where},
    Attribute, AttributeArgs, Expr, ExprParen, FnArg, ImplItem, ImplItemMethod, ItemImpl,
    ItemStruct, Meta, NestedMeta, PatType, Receiver, ReturnType, Signature, Token, Type,
    TypeGenerics, TypeParen, Visibility, WhereClause,
};
use zestors::{attrs::ZestorsAttrs, zestors_proc_macro_attr};

/// Derive the `Scheduler` trait for an actor.
#[proc_macro_derive(Scheduler, attributes(scheduler))]
pub fn derive_scheduler(item: TokenStream1) -> TokenStream1 {
    derive_scheduler::derive_scheduler(parse_macro_input!(item as ItemStruct))
        .map_or_else(|e| e.into_compile_error(), |v| v)
        .into()
}

/// Derive the `Scheduler` trait for an actor that does not do any scheduling.
#[proc_macro_derive(NoScheduler)]
pub fn derive_no_scheduler(item: TokenStream1) -> TokenStream1 {
    derive_scheduler::derive_no_scheduler(parse_macro_input!(item as ItemStruct))
        .map_or_else(|e| e.into_compile_error(), |v| v)
        .into()
}

/// Derive a custom address for your actor.
///
/// By default this will generate an address for actor `MyActor` named `MyActorAddr`.
/// To override this naming, use the #[addr(MyAdressName)] attribute.
#[proc_macro_derive(Addr, attributes(addr))]
pub fn derive_addr(item: TokenStream1) -> TokenStream1 {
    derive_addr::derive_addr(parse_macro_input!(item as ItemStruct))
        .map_or_else(|e| e.into_compile_error(), |v| v)
        .into()
}

/// Derive a default `Actor` trait for your actor. This uses sensible defaults, however for real use
/// it is recommended to implement the `Actor` trait manually.
#[proc_macro_derive(Actor)]
pub fn derive_actor(item: TokenStream1) -> TokenStream1 {
    derive_actor::derive_actor(parse_macro_input!(item as ItemStruct))
        .map_or_else(|e| e.into_compile_error(), |v| v)
        .into()
}

/// A macro used to generate methods on the actor's address straight from the handler functions.
#[proc_macro_attribute]
pub fn zestors(attrs: TokenStream1, item: TokenStream1) -> TokenStream1 {
    let impl_block = parse_macro_input!(item as ItemImpl);
    let attrs = parse_macro_input!(attrs as ZestorsAttrs);

    zestors_proc_macro_attr(attrs, impl_block)
        .map_or_else(|e| e.into_compile_error(), |v| v)
        .into()
}

pub(crate) mod kw {
    use syn::custom_keyword;
    custom_keyword!(addr);
    custom_keyword!(As);
    custom_keyword!(Inline);
    custom_keyword!(Ignore);
}
