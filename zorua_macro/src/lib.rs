use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{AttrStyle, Attribute, Data, DeriveInput, Index, Type};

#[proc_macro_derive(ZoruaField, attributes(alignment))]
pub fn zoruafield_derive_macro(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).unwrap(); //parse
    impl_zoruafield_trait(ast) //generate
}

fn impl_zoruafield_trait(ast: DeriveInput) -> TokenStream {
    let mut fields_impl = quote! {};
    let mut first_type_align = quote! { A1 };
    let mut declared_align: Option<Type> = None;
    for attr in &ast.attrs {
        if attr.path().is_ident("alignment") {
            match declared_align {
                None => declared_align = Some(attr.parse_args().expect(
                    "The `alignment` attribute must contain an alignment type, e.g. #[alignment(A4)]",
                )),
                Some(_) => panic!("There can only be 1 `alignment` attribute.")
            }
        }
    }

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

                if i > 1 && declared_align.is_none() {
                    panic!("Composite ZoruaFields must declare an `alignment` attribute.")
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
    let final_align = match declared_align {
        None => first_type_align,
        Some(ty) => quote! { #ty },
    };

    quote! {
        impl #impl_generics ZoruaField for #ident #type_generics #where_clause {
            type Alignment = #final_align;
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
