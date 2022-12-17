use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Data, DataStruct, Fields, FieldsNamed};

#[proc_macro_derive(CustomDebug)]
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

    let fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote!(.field(stringify!(#name), &self.#name))
    });

    Ok(quote!(
        impl ::std::fmt::Debug for #name {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#name))
                   #(#fields)*
                   .finish()
            }
        }
    ))
}
