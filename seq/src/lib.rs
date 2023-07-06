use proc_macro::TokenStream;
use proc_macro2::{Group, Literal, TokenStream as TokenStream2, TokenTree};
use syn::{braced, parse::Parse, parse_macro_input, Ident, LitInt, Result, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let s = parse_macro_input!(input as Seq);
    let mut tokens = TokenStream2::new();
    std::fs::write("seq_struct.rs", format!("{s:?}")).unwrap();
    for n in s.start..s.end {
        let content = replace_all(&s.ident, n, s.content.clone());
        tokens.extend(content);
    }

    TokenStream::from(tokens)
}

#[derive(Debug)]
struct Seq {
    ident: Ident,
    start: u16,
    end: u16,
    content: TokenStream2,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let content;
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?.base10_parse::<u16>()?;
        input.parse::<Token![..]>()?;
        let end = input.parse::<LitInt>()?.base10_parse::<u16>()?;
        braced!(content in input);
        let content = content.parse::<TokenStream2>()?;
        Ok(Seq {
            ident,
            start,
            end,
            content: content.into(),
        })
    }
}

fn replace_all(id: &Ident, n: u16, ts: TokenStream2) -> TokenStream2 {
    let content = ts.into_iter().map(|tt| match tt {
        TokenTree::Ident(i) if i == *id => TokenTree::Literal(Literal::u16_unsuffixed(n)),
        TokenTree::Group(g) => {
            let inner_ts = replace_all(id, n, g.stream());
            let mut new_g = Group::new(g.delimiter(), inner_ts);
            new_g.set_span(g.span());
            TokenTree::Group(new_g)
        }
        _ => tt,
    });
    content.collect()
}
