use proc_macro2::Ident;
use quote::ToTokens;
use syn::{
    braced, bracketed, parenthesized,
    parse::Parse,
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{For, Impl, Token},
    Attribute, Token, Visibility, WhereClause,
};

use crate::kw;

//------------------------------------------------------------------------------------------------
//  ZestorsAttrs
//------------------------------------------------------------------------------------------------

/// Attributes that can be put on a #[zestors] impl block
pub struct ZestorsAttrs {
    pub impl_expr: Option<(Impl, Ident)>,
    pub for_expr: Option<(For, Ident)>,
}

impl Parse for ZestorsAttrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(ZestorsAttrs {
            impl_expr: match input.parse::<Impl>() {
                Ok(impl_token) => Some((impl_token, input.parse::<Ident>()?)),
                Err(_) => None,
            },
            for_expr: match input.parse::<For>() {
                Ok(for_token) => Some((for_token, input.parse::<Ident>()?)),
                Err(_) => None,
            },
        })
    }
}

//------------------------------------------------------------------------------------------------
//  MethodAttr
//------------------------------------------------------------------------------------------------

pub struct ZestorsMethodAttrs {
    pub As: Option<AsAttr>,
    pub Ignore: Option<kw::Ignore>,
    pub Inline: Option<kw::Inline>,
}

impl ZestorsMethodAttrs {
    pub fn take_attrs_from(attrs: &mut Vec<Attribute>) -> syn::Result<Self> {
        let mut to_remove = Vec::new();

        let mut zestors_attrs = Self {
            As: None,
            Ignore: None,
            Inline: None,
        };

        for (i, attr) in attrs.iter().enumerate() {
            match attr.path.get_ident() {
                Some(ident) => match ident.to_string().as_str() {
                    "As" => {
                        zestors_attrs.As = {
                            to_remove.push(i);
                            Some(parse2(attr.to_token_stream())?)
                        }
                    }
                    "Ignore" => {
                        zestors_attrs.Ignore = {
                            to_remove.push(i);
                            Some(parse2(attr.path.to_token_stream())?)
                        }
                    }
                    "Inline" => {
                        zestors_attrs.Inline = {
                            to_remove.push(i);
                            Some(parse2(attr.path.to_token_stream())?)
                        }
                    }
                    _ => (),
                },
                None => (),
            }
        }

        for i in to_remove.iter().rev() {
            attrs.remove(*i);
        }

        Ok(zestors_attrs)
    }
}

pub struct AsAttr(pub kw::As, pub Visibility, pub Ident);
impl Parse for AsAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![#]>()?;
        let bracketed;
        bracketed!(bracketed in input);
        let As = bracketed.parse::<kw::As>()?;
        let parenthesized;
        parenthesized!(parenthesized in bracketed);
        let vis = parenthesized.parse::<Visibility>()?;
        let ident = parenthesized.parse::<Ident>()?;
        Ok(Self(As, vis, ident))
    }
}

//------------------------------------------------------------------------------------------------
//  keywords
//------------------------------------------------------------------------------------------------
