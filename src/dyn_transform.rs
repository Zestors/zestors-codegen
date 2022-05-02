use quote::ToTokens;
use std::collections::HashMap;
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Comma, FatArrow},
    Path, Token,
};

pub(crate) struct CheckDynTransform;

impl Parse for CheckDynTransform {
    fn parse(stream: ParseStream) -> syn::Result<Self> {
        // Parse left and right of arrow as input output
        let input;
        let output;
        bracketed!(input in stream);
        stream.parse::<FatArrow>()?;
        bracketed!(output in stream);

        // Parse these as punctuated sequences
        let input = Punctuated::<Path, Comma>::parse_terminated(&input)?;
        let output = Punctuated::<Path, Comma>::parse_terminated(&output)?;

        // Now map them to a `HashMap<String, Path>`
        let input = input
            .into_iter()
            .map(|path| (path.to_token_stream().to_string(), path))
            .collect::<HashMap<_, _>>();
        let output = output
            .into_iter()
            .map(|path| (path.to_token_stream().to_string(), path))
            .collect::<HashMap<_, _>>();


        // And finally check whether the input contains all keys of the output
        for out in &output {
            if !input.contains_key(out.0) {
                return Err(syn::Error::new_spanned(
                    out.1,
                    "Transformation is not allowed!",
                ));
            }
        }

        // And return Ok if this is true
        Ok(CheckDynTransform)
    }
}
