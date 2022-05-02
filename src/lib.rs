mod dyn_transform;

use dyn_transform::CheckDynTransform;
use proc_macro::TokenStream as TokenStream1;
use syn::parse_macro_input;

/// Checks whether the output items are a subset of the input items.
/// Should be called with `[a, b | c] => [a | ]` as input tokens.
#[proc_macro]
pub fn check_dyn_transform(item: TokenStream1) -> TokenStream1 {
    parse_macro_input!(item as CheckDynTransform);
    quote::quote!().into()
}
