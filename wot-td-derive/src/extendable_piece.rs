use proc_macro2::TokenStream;
use quote::quote;

pub fn expand_derive(input: &mut syn::DeriveInput) -> syn::Result<TokenStream> {
    let impl = quote!();
    todo!()
}
