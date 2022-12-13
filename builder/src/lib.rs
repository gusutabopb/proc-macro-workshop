use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Field, Type};

fn is_option(ty: &Type) -> bool {
    use syn::{Path, PathSegment, TypePath};
    match ty {
        Type::Path(TypePath {
            qself: None,
            path: Path { segments, .. },
        }) => match segments.first() {
            Some(PathSegment { ident, .. }) => ident == "Option",
            _ => false,
        },
        _ => false,
    }
}

type BuildTokens = (TokenStream2, TokenStream2, TokenStream2, TokenStream2);

fn regular_field(field: &Field) -> BuildTokens {
    let Field { vis, ty, ident, .. } = field;
    (
        quote!(#vis #ident: Option<#ty>),
        quote!(#ident: None),
        quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        ),
        quote!(
            #ident: self.#ident.take().ok_or(
                format!("Field missing: '{}'", stringify!(#ident))
            )?
        ),
    )
}

fn optional_field(field: &Field) -> BuildTokens {
    let Field { vis, ty, ident, .. } = field;
    (
        quote!(#vis #ident: Option<#ty>),
        quote!(#ident: None),
        quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        ),
        quote!(#ident: self.#ident.take().unwrap_or_default()),
    )
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: syn::DeriveInput = syn::parse(input).unwrap();
    let name = &input.ident;
    let builder = format_ident!("{}Builder", name);

    let mut defs = vec![];
    let mut inits = vec![];
    let mut setters = vec![];
    let mut validators = vec![];
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = input.data {
        for field in fields.iter() {
            // dbg!(&field);
            // Any field attributes are dropped
            let (def, init, set, validate) = match is_option(&field.ty) {
                true => optional_field(field),
                false => regular_field(field),
            };
            defs.push(def);
            inits.push(init);
            setters.push(set);
            validators.push(validate);
        }
    }

    quote!(
        pub struct #builder {
            #(#defs),*
        }

        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #(#inits),*
                }
            }
        }

        impl #builder {
            #(#setters)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#validators),*
                })
            }
        }
    )
    .into()
}
