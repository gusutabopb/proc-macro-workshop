use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    match derive_impl(input) {
        Ok(t) => t,
        Err(err) => err.to_compile_error(),
    }
    .into()
}

fn derive_impl(_input: TokenStream) -> Result<TokenStream2, syn::Error> {
    Ok(quote!())
}
