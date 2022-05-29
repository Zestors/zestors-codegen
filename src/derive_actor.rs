use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_quote,
    punctuated::Punctuated,
    token::{Brace, Colon, Gt, Lt, Semi, Struct},
    Attribute, Field, FieldsNamed, Generics, ItemStruct, Meta,
};

/// The #[derive(Addr)] macro
pub fn derive_actor(actor: ItemStruct) -> syn::Result<TokenStream> {
    let (actor_impl_generics, actor_ty_generics, actor_where_clause) =
        actor.generics.split_for_impl();
    let actor_ident = &actor.ident;

    Ok(quote! {
        #[zestors::core::async_trait]
        impl #actor_impl_generics zestors::core::Actor for #actor_ident #actor_ty_generics #actor_where_clause {
            type Init = Self;

            type Error = zestors::core::AnyhowError;

            type Exit = (Self, zestors::core::Signal<Self>);

            async fn initialize(init: Self::Init, addr: Self::Addr<zestors::core::Local>) -> zestors::core::InitFlow<Self> {
                zestors::core::InitFlow::Init(init)
            }

            fn handle_signal(self, signal: zestors::core::Signal<Self>, state: &mut zestors::core::State<Self>) -> zestors::core::SignalFlow<Self> {
                zestors::core::SignalFlow::Exit((self, signal))
            }
        }
    })
}
