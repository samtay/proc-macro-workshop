use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, DeriveInput, Error, Field, Fields,
    FieldsNamed, Ident, LitStr, Path, PathArguments, PathSegment, Type, TypePath,
};

// Get the inner type of the field, if there is one, e.g. T in Outer<T>. Else return None;
fn inner<'a>(outer_type: &str, field: &'a Field) -> Option<&'a Type> {
    let segments = match &field.ty {
        syn::Type::Path(TypePath {
            qself: None,
            path: Path { segments, .. },
        }) => Some(segments),
        _ => None,
    }?;
    let segment = segments.last()?;
    let args = match segment {
        PathSegment {
            ident,
            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        } if ident == outer_type => Some(args),
        _ => None,
    }?;
    // might as well catch a weird scenario using a different option type?
    (args.len() == 1).then_some(())?;
    let arg = args.last()?;
    match arg {
        syn::GenericArgument::Type(ty) => Some(ty),
        _ => None,
    }
}

// If the field is optional, return its inner type. Otherwise None.
fn optional(field: &Field) -> Option<&Type> {
    inner("Option", field)
}

// If the field is a Vec, return its inner type. Otherwise None.
fn vec_element_type(field: &Field) -> Option<&Type> {
    inner("Vec", field)
}

// If an each attribute is defined, return its name, else None.
fn each(field: &Field) -> Result<Option<String>, Error> {
    let mut name = None;
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("each") {
                    let value = meta.value()?;
                    let s: LitStr = value.parse()?;
                    name = Some(s.value());
                    Ok(())
                } else {
                    Err(meta.error("expected `builder(each = \"...\")`"))
                }
            })?
        }
    }
    Ok(name)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let buildername = format_ident!("{}Builder", name);
    let fields = match input.data {
        syn::Data::Struct(s) => match s.fields {
            Fields::Named(FieldsNamed { named, .. }) => named,
            _ => unimplemented!("Builder only supports structs with named fields"),
        },
        _ => unimplemented!("Builder only supports structs"),
    };

    let builder_fields = fields.iter().map(|field| {
        // we enforced named fields above
        let name = field.ident.as_ref().unwrap();
        // don't double wrap the option types
        let ty = optional(field).unwrap_or(&field.ty);
        if vec_element_type(field).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: ::std::option::Option<#ty> }
        }
    });
    let builder_fields_def = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap(); // we enforced named fields above
        if vec_element_type(field).is_some() {
            quote! { #name: ::std::vec::Vec::new() }
        } else {
            quote! { #name: ::std::option::Option::None }
        }
    });
    let whole_setters = fields.iter().filter_map(|field| {
        let name = field.ident.as_ref()?;
        let ty = optional(field).unwrap_or(&field.ty);
        match each(field) {
            Err(e) => Some(e.into_compile_error()),
            Ok(Some(each_name)) if name == &each_name => None,
            _ => Some(if vec_element_type(field).is_some() {
                quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = #name;
                        self
                    }
                }
            } else {
                quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }),
        }
    });
    let each_setters = fields.iter().filter_map(|field| {
        let name = field.ident.as_ref()?;
        // note: we emitted compiler errors above in case of Result::Err
        let each_name = each(field).ok().flatten()?;
        let vec_inner_type =
            vec_element_type(field).expect("`each` attribute is only available on Vec types");
        let each_name = syn::parse_str::<Ident>(&each_name).unwrap();
        Some(quote! {
            fn #each_name(&mut self, #each_name: #vec_inner_type) -> &mut Self {
                self.#name.push(#each_name);
                self
            }
        })
    });
    let inner_build_assignments = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap(); // we enforced named fields above
        if optional(field).is_some() || vec_element_type(field).is_some() {
            quote! {
                let #name = self.#name.clone();
            }
        } else {
            quote! {
                let #name = self.#name.clone().ok_or(
                    concat!("Attempted to build without setting ", stringify!(#name))
                )?;
            }
        }
    });
    let field_names = fields.iter().map(|f| f.ident.as_ref().unwrap());
    let build_fn = quote! {
        pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {

            #(#inner_build_assignments)*

            ::std::result::Result::Ok(#name { #(#field_names),* })
        }
    };

    let expanded = quote! {
        pub struct #buildername {
            #(#builder_fields),*
        }
        impl #buildername {
            #(#whole_setters)*

            #(#each_setters)*

            #build_fn
        }
        impl #name {
            pub fn builder() -> #buildername {
                #buildername {
                    #(#builder_fields_def),*
                }
            }
        }
    };

    let tokens = TokenStream::from(expanded);
    // eprintln!("TOKENS: {}", tokens);
    tokens
}
