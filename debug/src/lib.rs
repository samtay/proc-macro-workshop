use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, visit::Visit, Attribute, DataStruct, DeriveInput, Error,
    ExprLit, Field, Fields, FieldsNamed, GenericParam, Generics, Ident, Lit, LitStr, Result,
    TypePath, WherePredicate,
};

const ATTR_FMT_STR_IDENT: &str = "debug";

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let tokens2 = parse(input).unwrap_or_else(Error::into_compile_error);

    let tokens = TokenStream::from(tokens2);
    // eprintln!("TOKENS: {}", tokens);
    tokens
}

fn parse(input: DeriveInput) -> Result<proc_macro2::TokenStream> {
    let name = input.ident;
    let attributes = input.attrs;

    let fields = match input.data {
        syn::Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => named,
        _ => unimplemented!(),
    };
    let manual_bounds = find_manual_bounds(&attributes)?;

    let generics = add_trait_bounds(manual_bounds, input.generics, fields.iter());
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let debug_struct_field_calls = fields
        .iter()
        .map(|field| {
            let name = &field.ident;
            let fmt_str = find_fmt_str(&field)?.unwrap_or_else(|| "{:?}".to_string());
            Ok(quote! {
                .field(stringify!(#name), &format_args!(#fmt_str, &self.#name))
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let impltokens = quote! {
        impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#name))
                    #(#debug_struct_field_calls)*
                   .finish()
            }
        }
    };

    Ok(impltokens)
}

fn find_manual_bounds(attributes: &Vec<Attribute>) -> Result<Vec<WherePredicate>> {
    let mut bounds = Vec::new();
    for attr in attributes {
        if attr.path().is_ident("debug") {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("bound") {
                    let value = meta.value()?;
                    let s: LitStr = value.parse()?;
                    let bound = s.value();
                    bounds.push(syn::parse_str(&bound).unwrap());
                }
                Ok(())
            })?;
        }
    }
    Ok(bounds)
}

fn find_fmt_str(field: &Field) -> Result<Option<String>> {
    let mut fmt_str = None;
    for attr in &field.attrs {
        if attr.path().is_ident(ATTR_FMT_STR_IDENT) {
            let expr = &attr.meta.require_name_value()?.value;
            let s = match expr {
                syn::Expr::Lit(ExprLit {
                    lit: Lit::Str(ls), ..
                }) => Ok(ls.value()),
                _ => Err(Error::new_spanned(attr, "Expected a string literal")),
            }?;
            fmt_str = Some(s);
        }
    }
    Ok(fmt_str)
}

fn add_trait_bounds<'a, 'f>(
    manual_bounds: Vec<WherePredicate>,
    mut generics: Generics,
    fields: impl Iterator<Item = &'f Field> + Clone,
) -> Generics {
    if manual_bounds.is_empty() {
        let mut associated_types = vec![];
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                let mut v = DetectNonTrivial::new(&type_param.ident);
                for f in fields.clone() {
                    v.visit_field(&f);
                }
                if v.found_direct {
                    type_param.bounds.push(parse_quote!(::std::fmt::Debug));
                }
                associated_types.extend(v.found_associated.clone());
            }
        }
        for ass in associated_types {
            let wc = generics.make_where_clause();
            wc.predicates.push(parse_quote! {#ass : ::std::fmt::Debug});
        }
    } else {
        let wc = generics.make_where_clause();
        for bound in manual_bounds {
            wc.predicates.push(bound);
        }
    }
    generics
}

#[derive(Debug)]
struct DetectNonTrivial<'g, 'f> {
    /// Ident of the generic type param we are searching for
    ident: &'g Ident,
    /// Did we find a nontrivial usage of generic T itself? e.g. `field: (T, String)`
    found_direct: bool,
    /// Did we find a nontrivial usage of associated type of T? e.g. `field:: T::Val`
    found_associated: Vec<&'f TypePath>,
}

impl<'g, 'f> DetectNonTrivial<'g, 'f> {
    fn new(ident: &'g Ident) -> Self {
        Self {
            ident,
            found_direct: false,
            found_associated: vec![],
        }
    }
}

impl<'g, 'ast> Visit<'ast> for DetectNonTrivial<'g, 'ast> {
    fn visit_type_path(&mut self, ty: &'ast TypePath) {
        // If this is a phantomdata path, no need to search anything nested in here
        if ty
            .path
            .segments
            .last()
            .filter(|s| s.ident == "PhantomData")
            .is_some()
        {
            return;
        }

        // If the entire path is the generic param, this is a direct usage
        if ty.path.is_ident(self.ident) {
            self.found_direct = true;
        }

        // If the path starts with generic param and has length greater than 1,
        // then this is associated type usage: e.g. T::Val
        if ty
            .path
            .segments
            .first()
            .filter(|s| &s.ident == self.ident)
            .is_some()
            && ty.path.segments.len() > 1
        {
            self.found_associated.push(ty);
        }

        if let Some(it) = &ty.qself {
            self.visit_qself(it);
        }

        self.visit_path(&ty.path);
    }
}
