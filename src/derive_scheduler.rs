use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{Fields, ItemStruct};

pub fn derive_no_scheduler(actor: ItemStruct) -> syn::Result<TokenStream> {
    let (impl_generics, ty_generics, where_clause) = actor.generics.split_for_impl();
    let actor_ident = &actor.ident;

    Ok(quote! {
        impl #impl_generics Unpin for #actor_ident #ty_generics #where_clause {}

        impl #impl_generics zestors::core::Stream for #actor_ident #ty_generics #where_clause {
            type Item = zestors::core::Action<Self>;

            fn poll_next(
                self: std::pin::Pin<&mut Self>,
                cx: &mut std::task::Context<'_>,
            ) -> std::task::Poll<Option<Self::Item>> {
                std::task::Poll::Ready(None)
            }
        }
    })
}

pub fn derive_scheduler(actor: ItemStruct) -> syn::Result<TokenStream> {
    let (impl_generics, ty_generics, where_clause) = actor.generics.split_for_impl();
    let ident = &actor.ident;

    let scheduler_field = if let Fields::Named(fields) = actor.fields {
        let fields = fields
            .named
            .iter()
            .filter(|field| {
                field
                    .attrs
                    .iter()
                    .find(|attr| attr.path.is_ident("scheduler"))
                    .is_some()
            }).collect::<Vec<_>>();

        match fields.len() {
            0 => Err(scheduler_err("Add #[scheduler] attribute.")),
            1 => Ok(fields.first().unwrap().ident.clone().unwrap()),
            _ => Err(scheduler_err("More than one #[scheduler] attribute found."))
        }
    } else {
        Err(scheduler_err("Only named structs are currently supported."))
    }?;

    Ok(quote! {
        impl #impl_generics Unpin for #ident #ty_generics #where_clause {}

        impl #impl_generics zestors::core::Stream for #ident #ty_generics #where_clause {
            type Item = zestors::core::Action<Self>;

            fn poll_next(
                mut self: std::pin::Pin<&mut Self>,
                cx: &mut std::task::Context<'_>,
            ) -> std::task::Poll<Option<Self::Item>> {
                use zestors::core::StreamExt;
                self.#scheduler_field.poll_next_unpin(cx)
            }
        }
    })
}

fn scheduler_err(str: &'static str) -> syn::Error {
    syn::Error::new(
        Span::call_site(), 
        format!(
            "{}\n\n-- Usage --\n\n#[derive(Scheduler)]\nstruct MyActor {{ \n    field1: ..., \n    [#scheduler] \n    field2: ..., \n    ...\n}}", 
            str, 
            
        )
    )
}
