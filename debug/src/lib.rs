use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, Data, DataStruct, Field, Fields, FieldsNamed, PathSegment};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    match derive_impl(input) {
        Ok(t) => t.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn derive_impl(input: TokenStream) -> Result<TokenStream2, syn::Error> {
    let input: syn::DeriveInput = syn::parse(input)?;
    let name = &input.ident;

    let Data::Struct(
        DataStruct { fields: Fields::Named(FieldsNamed { named: fields, .. } ), .. }
    ) = input.data else {
        return Err(syn::Error::new(input.span(), "expected struct with fields"));
    };

    let fields: Vec<_> = fields
        .iter()
        .map(|field| {
            let Field { ident, attrs, .. } = field;
            match attrs.first() {
                None => Ok(quote!(.field(stringify!(#ident), &self.#ident))),
                Some(attr) => {
                    let _fmt = parse_attr(attr)?;
                    Ok(quote!(.field(stringify!(#ident), &format_args!(#_fmt, &self.#ident))))
                }
            }
        })
        .collect::<Result<Vec<_>, syn::Error>>()?;

    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();
    let where_clause = generate_where_clause(&input.generics);

    Ok(quote!(
        impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#name))
                   #(#fields)*
                   .finish()
            }
        }
    ))
}

fn parse_attr(attr: &Attribute) -> Result<String, syn::Error> {
    use syn::{Lit, Meta::NameValue, MetaNameValue};
    let meta = attr.parse_meta()?;
    let err = Err(syn::Error::new(meta.span(), "failed to parse attribute"));
    let NameValue(MetaNameValue { path, lit, ..}) = &meta else {
        return err;
    };
    let Some( PathSegment { ident, .. } ) = path.segments.first() else {
        return err;
    };
    match (ident.to_string().as_str(), lit) {
        ("debug", Lit::Str(s)) => Ok(s.value()),
        _ => err,
    }
}

fn generate_where_clause(generics: &syn::Generics) -> TokenStream2 {
    let clauses: Vec<_> = generics
        .type_params()
        .map(|syn::TypeParam { ident, bounds, .. }| {
            if bounds.is_empty() {
                quote!(#ident: ::std::fmt::Debug)
            } else {
                quote!(#ident: #bounds + ::std::fmt::Debug)
            }
        })
        .collect();

    if clauses.is_empty() {
        quote!()
    } else {
        quote!(where #(#clauses),*)
    }
}
