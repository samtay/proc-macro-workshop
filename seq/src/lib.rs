use std::{fmt::Display, iter::Peekable};

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
        let mut prefix_with = None::<Ident>;
        let mut stream = ts.into_iter().peekable();
        loop {
            match (prefix_with.as_ref(), stream.next()) {
                // Replace x~N => xn
                (Some(prefix), Some(TokenTree::Ident(i))) if i == self.ident => {
                    // Replace x~N~y => xny
                    let suffix = if Self::advance_tilde(&mut stream) {
                        match stream.next() {
                            Some(TokenTree::Ident(s)) => Some(s),
                            unexpected => {
                                return Err(Self::unexpected_err(
                                    unexpected,
                                    "Expected suffix identifier after `~`",
                                ));
                            }
                        }
                    } else {
                        None
                    };
                    out.push(TokenTree::Ident(Ident::new(
                        &format!(
                            "{}{}{}",
                            prefix,
                            n,
                            suffix.map(|s| s.to_string()).unwrap_or_else(String::new)
                        ),
                        prefix.span(),
                    )));
                    prefix_with = None;
                }
                // Err x~(!N)
                (Some(_), unexpected) => {
                    return Err(Self::unexpected_err(
                        unexpected,
                        format!("Expected {} after `~`", self.ident),
                    ));
                }
                // Replace N => n
                (None, Some(TokenTree::Ident(i))) if i == self.ident => {
                    out.push(TokenTree::Literal(Literal::u16_unsuffixed(n)));
                }
                // Check if ~ exists after ident, if so prepare to prefix
                (None, Some(TokenTree::Ident(i))) => {
                    if Self::advance_tilde(&mut stream) {
                        // Prepare to paste (see first case above)
                        prefix_with = Some(i);
                    } else {
                        // Otherwise stick this ident in unchanged
                        out.push(TokenTree::Ident(i));
                    }
                }
                // Recusrively interpolate
                (None, Some(TokenTree::Group(g))) => {
                    let inner_ts = self.interpolate(n, g.stream())?;
                    let mut new_g = Group::new(g.delimiter(), inner_ts);
                    new_g.set_span(g.span());
                    out.push(TokenTree::Group(new_g));
                }
                // Base case: copy stream as is
                (None, Some(tt)) => out.push(tt),
                // Stream is finished
                (None, None) => break,
            }
        }

        Ok(out.into_iter().collect())
    }

    /// If the next character is a tilde, skip it and return true
    fn advance_tilde<I>(stream: &mut Peekable<I>) -> bool
    where
        I: Iterator<Item = TokenTree>,
    {
        match stream.peek() {
            Some(TokenTree::Punct(p)) if p.as_char() == '~' => {
                // skip the tilde in the outer loop
                stream.next();
                true
            }
            _ => false,
        }
    }

    /// Produce a compiler error for an invalid or missing token
    fn unexpected_err(opt_tt: Option<TokenTree>, message: impl Display) -> Error {
        Error::new(opt_tt.map_or_else(Span::call_site, |tt| tt.span()), message)
    }
}
