use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Attribute, Field, PathSegment, Type};

#[derive(Debug)]
enum FieldType {
    Raw,
    Option(Type),
    Repeated(Type),
}

fn field_type(ty: &Type) -> FieldType {
    use syn::{
        AngleBracketedGenericArguments as ABGA, GenericArgument, Path,
        PathArguments::AngleBracketed, TypePath,
    };
    let Type::Path(TypePath { qself: None, path: Path { segments, .. } }) = ty else {
        return FieldType::Raw;
    };
    let Some(
        PathSegment { ident, arguments: AngleBracketed( ABGA { args, .. } ) }
    ) = segments.first() else {
        return FieldType::Raw;
    };
    let Some(GenericArgument::Type(t)) = args.first() else {
        return FieldType::Raw;
    };
    match ident.to_string().as_str() {
        "Option" => FieldType::Option(t.clone()),
        "Vec" => FieldType::Repeated(t.clone()),
        _ => FieldType::Raw,
    }
}

type BuildTokens = (TokenStream2, TokenStream2, TokenStream2, TokenStream2);

fn raw_field(field: &Field) -> BuildTokens {
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

fn optional_field(field: &Field, ty: Type) -> BuildTokens {
    let Field { vis, ident, .. } = field;
    (
        quote!(#vis #ident: Option<#ty>),
        quote!(#ident: None),
        quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        ),
        quote!(#ident: self.#ident.take()),
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
            let (def, init, set, validate) = match field_type(&field.ty) {
                FieldType::Option(ty) => optional_field(field, ty),
                FieldType::Repeated(_) => raw_field(field),
                FieldType::Raw => raw_field(field),
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
