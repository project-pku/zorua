use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Index};

#[proc_macro_derive(ZoruaField)]
pub fn zoruafield_derive_macro(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).unwrap(); //parse
    impl_zoruafield_trait(ast) //generate
}

fn impl_zoruafield_trait(ast: DeriveInput) -> TokenStream {
    let mut fields_impl = quote! {};
    let mut tuple_field_count: usize = 0;
    match &ast.data {
        // Ensure the input is a struct
        Data::Struct(data) => {
            for field in &data.fields {
                fields_impl.extend(match &field.ident {
                    // Struct fields
                    Some(field_ident) => {
                        quote! {
                            ZoruaField::swap_bytes_mut(&mut self.#field_ident);
                        }
                    }
                    // Tuple fields
                    None => {
                        let index = Index::from(tuple_field_count);
                        let tokens = quote! {
                            ZoruaField::swap_bytes_mut(&mut self.#index);
                        };
                        tuple_field_count += 1;
                        tokens
                    }
                });
            }
        }
        _ => panic!("#[derive(ZoruaField)] is only defined for structs"),
    }

    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    quote! {
        impl #impl_generics ZoruaField for #ident #type_generics #where_clause {
            fn swap_bytes_mut(&mut self) {
                #fields_impl
            }
        }
    }
    .into()
}
