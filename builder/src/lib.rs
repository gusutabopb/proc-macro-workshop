use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
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

fn build_attribute(attr: &Attribute) -> Result<String, syn::Error> {
    use syn::{Lit, Meta::List, Meta::NameValue, MetaList, MetaNameValue, NestedMeta};
    let meta = attr.parse_meta()?;
    let err = Err(syn::Error::new(meta.span(), "failed to parse attribute"));
    let List(MetaList { path, nested, ..}) = &meta else {
        return err;
    };
    let Some( PathSegment { ident, .. } ) = path.segments.first() else {
        return err;
    };
    if ident != "builder" {
        return err;
    }
    let Some(
        NestedMeta::Meta(NameValue(MetaNameValue { path, lit, .. } ) )
    ) = nested.first() else {
        return err;
    };
    let Some( PathSegment { ident, .. } ) = path.segments.first() else {
        return err;
    };
    if ident != "each" {
        // ident.span() gives a better error span, but "fails" test
        // Span contents may change depending on toolchain.
        // This passes errors when using 1.68.0-nightly-2022-12-12, but fails with 1.66.0-stable
        return Err(syn::Error::new(
            meta.span(),
            "expected `builder(each = \"...\")`",
        ));
    }
    let Lit::Str(s) = lit else {
        return err;
    };
    Ok(s.value())
}

type BuildTokens = (TokenStream2, TokenStream2, TokenStream2, TokenStream2);
type FieldParseResult = Result<BuildTokens, syn::Error>;

fn raw_field(field: &Field) -> FieldParseResult {
    let Field { vis, ty, ident, .. } = field;
    Ok((
        quote!(#vis #ident: std::option::Option<#ty>),
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
    ))
}

fn repeated_field(field: &Field, ty: Type) -> FieldParseResult {
    let Field {
        attrs, vis, ident, ..
    } = field;
    let Some(attr) = attrs.first() else {
        // No attributes - return raw
        return raw_field(field)
    };
    let each = format_ident!("{}", build_attribute(attr)?);

    let all_in_one_builder = if each == ident.clone().unwrap() {
        quote!()
    } else {
        quote!(
            fn #ident(&mut self, #ident: Vec<#ty>) -> &mut Self {
                self.#ident.extend(#ident);
                self
            }
        )
    };

    Ok((
        quote!(#vis #ident: Vec<#ty>),
        quote!(#ident: Vec::new()),
        quote!(
            fn #each(&mut self, #each: #ty) -> &mut Self {
                self.#ident.push(#each);
                self
            }

            #all_in_one_builder
        ),
        quote!(#ident: self.#ident.to_owned()),
    ))
}

fn optional_field(field: &Field, ty: Type) -> FieldParseResult {
    let Field { vis, ident, .. } = field;
    Ok((
        quote!(#vis #ident: std::option::Option<#ty>),
        quote!(#ident: None),
        quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        ),
        quote!(#ident: self.#ident.take()),
    ))
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    match derive_impl(input) {
        Ok(t) => t,
        Err(err) => err.to_compile_error(),
    }
    .into()
}

fn derive_impl(input: TokenStream) -> Result<TokenStream2, syn::Error> {
    let input: syn::DeriveInput = syn::parse(input)?;
    let name = &input.ident;
    let builder = format_ident!("{}Builder", name);

    let mut defs = vec![];
    let mut inits = vec![];
    let mut setters = vec![];
    let mut validators = vec![];
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = input.data {
        for field in fields.iter() {
            // dbg!(&field);
            let (def, init, set, validate) = match field_type(&field.ty) {
                FieldType::Option(ty) => optional_field(field, ty)?,
                FieldType::Repeated(ty) => repeated_field(field, ty)?,
                FieldType::Raw => raw_field(field)?,
            };
            defs.push(def);
            inits.push(init);
            setters.push(set);
            validators.push(validate);
        }
    }

    Ok(quote!(
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

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#validators),*
                })
            }
        }
    ))
}
