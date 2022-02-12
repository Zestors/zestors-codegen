use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Attribute, ItemStruct};

/// ## What it does
/// Derive a custom address for an actor. This derive macro does the following:
/// * Create a unit struct: `{Actor}Address(Address<{Actor}>);`
/// * Implement `From<Address<{Actor}>>` for this address.
/// * Implement `RawAddress<Actor = {Actor}>` for this address.
/// * `#[derive(Debug, Clone)]` for this address.
///
/// The name of the struct can be changed by adding the optional attribute
/// `#[address({AddressName})]` after the `#[derive]` usage.
///
/// ## Example
/// ```ignore
/// #[derive(Address)]
/// #[address(MyActorAddress)] // this is optional
/// struct MyActor {
///     ...
/// }
///
/// impl Actor for MyActor {
///     type Address = MyActorAddress;
///     ...
/// }
/// ```
///
/// ## Important!
/// Don't forget to set [zestors::Actor::Address] to the new address!.
///
/// ## Addressable
/// If you would like to be able to directly call methods like `.msg()` or `.req()`
/// on your custom address, then you will probably want to `#[derive(Addressable)]`
/// as well.
#[proc_macro_derive(Address, attributes(address))]
pub fn derive_address(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as ItemStruct);

    let actor_name = item.ident;
    let vis = item.vis;
    let address_name = match address_name(item.attrs.first(), &actor_name) {
        Ok(name) => name,
        Err(e) => return e.into_compile_error().into(),
    };

    let comment = format!("This address belongs to the actor: [{}].", actor_name);

    quote! {
        /// An automatically generated address using [zestors::derive::Address].
        ///
        #[doc = #comment]
        #[derive(Debug, Clone)]
        #vis struct #address_name(zestors::Address<#actor_name>);

        impl From<zestors::Address<#actor_name>> for #address_name {
            fn from(address: zestors::Address<#actor_name>) -> Self {
                Self(address)
            }
        }

        impl zestors::RawAddress for #address_name {
            fn raw_address(&self) -> &zestors::Address<#actor_name> {
                &self.0
            }

            type Actor = #actor_name;
        }
    }
    .into()
}

/// ## Description
/// This macro can be used to derive `zestors::Addressable`. This means that you can now
/// directly call methods like `.msg()` or `.req()` on this address.
///
/// It can only be used in combination with `[#derive(Address)]`, and should be used on
/// the struct definition of the actor, **not** on the struct definition of the address.
///
/// /// ## Example
/// ```ignore
/// #[derive(Address, Addressable)]
/// struct MyActor {
///     ...
/// }
/// ```
#[proc_macro_derive(Addressable, attributes(address))]
pub fn derive_addressabe(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as ItemStruct);

    let actor_name = item.ident;
    let address_name = match address_name(item.attrs.first(), &actor_name) {
        Ok(name) => name,
        Err(e) => return e.into_compile_error().into(),
    };

    quote! {
        impl zestors::Addressable<#actor_name> for #address_name {}
    }
    .into()
}

fn address_name(attr: Option<&Attribute>, actor_name: &Ident) -> Result<Ident, syn::Error> {
    match attr {
        Some(attr) => match attr.parse_meta().unwrap() {
            syn::Meta::Path(path) => Err(syn::Error::new_spanned(
                path,
                "Should be the identifier of the custom address",
            )),
            syn::Meta::List(list) => match list.nested.first().unwrap() {
                syn::NestedMeta::Meta(meta) => match meta {
                    syn::Meta::Path(path) => Ok(Ident::new(
                        &format!("{}", path.segments.first().unwrap().ident),
                        path.segments.first().unwrap().span(),
                    )),
                    _ => Err(syn::Error::new_spanned(
                        meta,
                        "Should be the identifier of the custom address",
                    )),
                },
                syn::NestedMeta::Lit(lit) => Err(syn::Error::new_spanned(
                    lit,
                    "Should be the identifier of the custom address",
                )),
            },
            syn::Meta::NameValue(name) => Err(syn::Error::new_spanned(
                name,
                "Should be the identifier of the custom address",
            )),
        },
        None => Ok(Ident::new(
            &format!("{}Address", actor_name),
            actor_name.span(),
        )),
    }
}
