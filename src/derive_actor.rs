use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{Attribute, Field, FieldsNamed, Generics, ItemStruct, Meta};

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

            type Halt = ();

            type Exit = (Self, zestors::core::Event<Self>);

            async fn initialize(init: Self::Init, addr: Self::Addr) -> zestors::core::InitFlow<Self> {
                zestors::core::InitFlow::Init(init)
            }

            fn handle_event(self, event: zestors::core::Event<Self>, state: &mut zestors::core::State<Self>) -> zestors::core::EventFlow<Self> {
                zestors::core::EventFlow::Exit((self, event))
            }
        }
    })
}
