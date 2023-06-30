mod extendable_piece;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(ExtendablePiece, attributes(extendable_piece))]
pub fn derive_extendable_piece(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    extendable_piece::expand_derive(&mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
