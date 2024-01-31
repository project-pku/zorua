use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{AttrStyle, Attribute, Data, DeriveInput, Index};

#[proc_macro_derive(ZoruaField)]
pub fn zoruafield_derive_macro(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).unwrap(); //parse
    impl_zoruafield_trait(ast) //generate
}

fn impl_zoruafield_trait(ast: DeriveInput) -> TokenStream {
    let mut fields_impl = quote! {};
    let mut first_type_align = quote! { A1 };

    match &ast.data {
        // Ensure the input is a struct
        Data::Struct(data) => {
            for (i, field) in data.fields.iter().enumerate() {
                let field_ty = &field.ty;
                if i == 0 {
                    first_type_align = quote! {
                        <#field_ty as ZoruaField>::Alignment
                    };
                }

                //Build up swap_bytes_mut fn
                fields_impl.extend(match &field.ident {
                    // Struct fields
                    Some(field_ident) => {
                        quote! {
                            ZoruaField::swap_bytes_mut(&mut self.#field_ident);
                        }
                    }
                    // Tuple fields
                    None => {
                        let index = Index::from(i);
                        let tokens = quote! {
                            ZoruaField::swap_bytes_mut(&mut self.#index);
                        };
                        tokens
                    }
                });

                //temporary, until align attribute added.
                if i > 1 {
                    panic!("#[derive(ZoruaField)] does not support composite structs")
                }
                if has_repr(&ast.attrs, "align") {
                    panic!("#[derive(ZoruaField)] does not support structs with an `align` repr")
                }
            }
        }
        _ => panic!("#[derive(ZoruaField)] is only defined for structs"),
    }

    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    quote! {
        impl #impl_generics ZoruaField for #ident #type_generics #where_clause {
            type Alignment = #first_type_align;
            fn swap_bytes_mut(&mut self) {
                #fields_impl
            }
        }
    }
    .into()
}

fn has_repr(attrs: &[Attribute], repr: &str) -> bool {
    for attr in attrs {
        // If the style isn't outer, reject it
        if !matches!(attr.style, AttrStyle::Outer) {
            continue;
        }
        // If it doesn't match, reject it
        if !format!("{}", attr.to_token_stream()).starts_with(&format!("#[repr({}", repr)) {
            continue;
        }

        return true;
    }
    false
}
