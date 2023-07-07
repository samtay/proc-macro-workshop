use proc_macro::TokenStream;
use proc_macro2::{Group, Literal, Span, TokenStream as TokenStream2, TokenTree};
use syn::{braced, parse::Parse, parse_macro_input, Error, Ident, LitInt, Result, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let s = parse_macro_input!(input as Seq);
    // std::fs::write("seq_struct.rs", format!("{s:?}")).unwrap();
    let tokens = s.output().unwrap_or_else(Error::into_compile_error);
    std::fs::write("tokens.rs", format!("{tokens:?}")).unwrap();
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

impl Seq {
    fn output(&self) -> Result<TokenStream2> {
        let mut out = TokenStream2::new();
        for n in self.start..self.end {
            let content = self.interpolate(n, self.content.clone())?;
            out.extend(content);
        }
        Ok(out)
    }

    fn interpolate(&self, n: u16, ts: TokenStream2) -> Result<TokenStream2> {
        let mut out = vec![];
        let mut paste_with = None::<Ident>;
        let mut stream = ts.into_iter().peekable();
        loop {
            match (paste_with.as_ref(), stream.next()) {
                // Replace x~N => xn
                (Some(prefix), Some(TokenTree::Ident(i))) if i == self.ident => {
                    out.push(TokenTree::Ident(Ident::new(
                        &format!("{}{}", prefix, n),
                        prefix.span(),
                    )));
                    paste_with = None;
                }
                // Err x~(!N)
                (Some(_), unexpected) => {
                    return Err(Error::new(
                        unexpected.map_or_else(Span::call_site, |u| u.span()),
                        format!("Expected {} after `~`", self.ident),
                    ))
                }
                // Replace N => n
                (None, Some(TokenTree::Ident(i))) if i == self.ident => {
                    out.push(TokenTree::Literal(Literal::u16_unsuffixed(n)));
                }
                (None, Some(TokenTree::Ident(i))) => {
                    match stream.peek() {
                        // Prepare to paste (see first case above)
                        Some(TokenTree::Punct(p)) if p.as_char() == '~' => {
                            paste_with = Some(i);
                            // skip the tilde in the outer loop
                            stream.next();
                        }
                        // Otherwise stick this ident in unchanged
                        _ => out.push(TokenTree::Ident(i)),
                    }
                }
                (None, Some(TokenTree::Group(g))) => {
                    let inner_ts = self.interpolate(n, g.stream())?;
                    let mut new_g = Group::new(g.delimiter(), inner_ts);
                    new_g.set_span(g.span());
                    out.push(TokenTree::Group(new_g));
                }
                (None, Some(tt)) => out.push(tt),
                (None, None) => break,
            }
        }

        Ok(out.into_iter().collect())
    }
}
