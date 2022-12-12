use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::Field;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: syn::DeriveInput = syn::parse(input).unwrap();
    let struct_name = &input.ident;
    let struct_builder = format_ident!("{}Builder", struct_name);

    let mut field_def = vec![];
    let mut field_init = vec![];
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = input.data {
        for field in fields.iter() {
            dbg!(&field);
            // Any field attributes are dropped
            let Field { vis, ty, ident, .. } = field;
            field_def.push(quote!(#vis #ident: Option<#ty>));
            field_init.push(quote!(#ident: None));
        }
    }

    quote!(
        pub struct #struct_builder {
            #(#field_def),*
        }

        impl #struct_name {
            pub fn builder() -> #struct_builder {
                #struct_builder {
                    #(#field_init),*
                }
            }
        }
    )
    .into()
}
