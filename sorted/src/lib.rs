use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Error, Item, ItemEnum, Result, Variant};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    // Always return the given input stream
    let mut tokens = input.clone();
    let item = parse_macro_input!(input as Item);
    if let Err(e) = sorted_inner(item) {
        tokens.extend(TokenStream::from(Error::into_compile_error(e)))
    }
    // eprintln!("TOKENS: {}", tokens);
    tokens
}

// Convenience wrapper around all the logic so that we can propagate compiler errors via ? to the
// top level
fn sorted_inner(item: Item) -> Result<proc_macro2::TokenStream> {
    let enumitem = parse_enum(item)?;
    ensure_sorted(enumitem.variants.iter())?;
    Ok(quote! {#enumitem})
}

fn parse_enum(item: Item) -> Result<ItemEnum> {
    match item {
        Item::Enum(e) => Ok(e),
        _ => Err(Error::new(
            // This let's us keep the compiler error focused on the #[sorted] attribute
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn ensure_sorted<'a>(variants: impl Iterator<Item = &'a Variant>) -> Result<()> {
    let mut sorted_variants: Vec<&Variant> = vec![];
    for variant in variants {
        if let Some(prev) = &sorted_variants.last() {
            if variant.ident < prev.ident {
                for v in &sorted_variants {
                    if variant.ident < v.ident {
                        return Err(Error::new(
                            variant.span(),
                            format!("{} should sort before {}", variant.ident, v.ident),
                        ));
                    }
                }
            }
        }
        sorted_variants.push(variant);
    }
    Ok(())
}
