use std::{borrow::Cow, fmt::Display};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, spanned::Spanned, visit_mut::VisitMut, Error, ExprMatch, Item, ItemEnum,
    ItemFn, Path, Result,
};

/// An attribute for enums that enforces sorted variants
#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    // Always return the given input stream
    let mut tokens = input.clone();
    let item = parse_macro_input!(input as Item);

    // And add compiler errors if necessary
    if let Err(e) = sorted_inner(item) {
        tokens.extend(TokenStream::from(Error::into_compile_error(e)))
    }
    // eprintln!("TOKENS: {}", tokens);
    tokens
}

/// Convenience wrapper around all the logic so that we can propagate compiler errors via ? to the
/// top level
fn sorted_inner(item: Item) -> Result<()> {
    let enumitem = parse_enum(item)?;
    ensure_sorted(enumitem.variants.iter().map(|v| &v.ident))?;
    Ok(())
}

/// Ensure `Item` is an enum
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

/// Ensure spannable items are sorted (used for variant idents in enum declaration)
fn ensure_sorted<'a, I, T: 'a>(elems: I) -> Result<()>
where
    I: Iterator<Item = &'a T>,
    T: PartialOrd + ToTokens + Spanned + Display,
{
    ensure_sorted_by(elems, |e| e.to_string())
}

// N.B. ToTokens is required for `new_spanned`, which allows one to target the full path, e.g.
// `Error::Fmt`. Using `Error::new(path.span(), ..)` only targets the first segment of the path.
fn ensure_sorted_by<'a, I, T: 'a, F>(elems: I, f: F) -> Result<()>
where
    I: Iterator<Item = &'a T>,
    T: Spanned + ToTokens,
    F: Fn(&T) -> String,
{
    let mut sorted_elems: Vec<&T> = vec![];
    for elem in elems {
        for v in &sorted_elems {
            if f(elem) < f(v) {
                return Err(Error::new_spanned(
                    elem,
                    format!("{} should sort before {}", f(elem), f(v)),
                ));
            }
        }
        sorted_elems.push(elem);
    }
    Ok(())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut item_fn = parse_macro_input!(input as ItemFn);
    let mut check = Check::default();

    // remove the `#[sorted]` attribute, and gather any sorting errors
    check.visit_item_fn_mut(&mut item_fn);

    // output tokens from the modified `item_fn`
    let mut tokens = TokenStream::from(quote! {#item_fn});

    // add a compiler error if present
    if let Some(e) = check.err {
        tokens.extend(TokenStream::from(Error::into_compile_error(e)))
    }

    tokens
}

#[derive(Default)]
struct Check {
    err: Option<Error>,
}

impl VisitMut for Check {
    fn visit_expr_match_mut(&mut self, m: &mut ExprMatch) {
        // By keeping `check_requested` outside of the visitor, and defined per match expression,
        // we allow users to specify `#[sorted]` granularly, rather than forcing nested matches to
        // all be sorted.
        let mut check_requested = false;

        // Detected sorted request and remove it
        m.attrs.retain(|a| {
            if a.path().is_ident("sorted") {
                check_requested = true;
                false
            } else {
                true
            }
        });

        // For consistency's sake, go visit the other remaining attrs
        for a in &mut m.attrs {
            self.visit_attribute_mut(a);
        }

        self.visit_expr_mut(&mut *m.expr);

        // Ensure arms are sorted
        if check_requested {
            let n = m.arms.len();
            self.err = m
                .arms
                .iter()
                .enumerate()
                .filter_map(|(i, a)| match &a.pat {
                    syn::Pat::Path(p) => Some(Ok(Cow::Borrowed(&p.path))),
                    syn::Pat::Struct(s) => Some(Ok(Cow::Borrowed(&s.path))),
                    syn::Pat::TupleStruct(t) => Some(Ok(Cow::Borrowed(&t.path))),
                    syn::Pat::Ident(i) => Some(Ok(Cow::Owned(i.ident.clone().into()))),
                    syn::Pat::Wild(_) => (i != n - 1)
                        .then(|| Err(Error::new_spanned(&a.pat, "wildcard _ should be last"))),
                    _ => Some(Err(Error::new_spanned(&a.pat, "unsupported by #[sorted]"))),
                })
                .collect::<Result<_>>()
                .and_then(ensure_sorted_arms)
                .err();
        }

        for a in &mut m.arms {
            self.visit_arm_mut(a);
        }
    }
}

fn ensure_sorted_arms(paths: Vec<Cow<Path>>) -> Result<()> {
    ensure_sorted_by(paths.iter(), |p| {
        let mut s = quote!(#p).to_string();
        s.retain(|c| !c.is_whitespace());
        s
    })
}
