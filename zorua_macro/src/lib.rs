use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parser, AttrStyle, Attribute, Data, DeriveInput, Index, Type};

#[proc_macro_derive(ZoruaField, attributes(alignment))]
pub fn zoruafield_derive_macro(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).unwrap(); //parse
    impl_zoruafield_trait(ast) //generate
}

/// On top of implementing the ZoruaFallible trait, this derive macro
/// also includes a [TryInto]`<Self>` impl for [Fallible]`<Self, B>`.
#[proc_macro_derive(ZoruaFallible, attributes(targets))]
pub fn zoruafallible_derive_macro(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).unwrap(); //parse
    impl_zoruafallible_trait(ast) //generate
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

fn impl_zoruafallible_trait(ast: DeriveInput) -> TokenStream {
    let mut targets: Option<Vec<Type>> = None;
    for attr in &ast.attrs {
        if attr.path().is_ident("targets") {
            let data = syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::parse_terminated
                .parse2(attr.parse_args().expect(
                    "The `targets` attribute must contain a list of target types for ZoruaFallible, e.g. #[targets(u3, u8)]",
                ))
                .unwrap();

            if !data.is_empty() {
                targets = Some(Vec::from_iter(data));
            }
        }
    }

    let ident = ast.ident;
    let mut is_valid_impl = quote! {};
    match &ast.data {
        Data::Enum(data) => {
            for variant in &data.variants {
                if !variant.fields.is_empty() {
                    panic!("ZoruaFallible only supports c-like enums (i.e. no fields).")
                }
                let variant_ident = &variant.ident;
                is_valid_impl.extend(quote! {
                    || (value == unsafe {std::mem::transmute(#ident::#variant_ident)})
                });
            }
        }
        _ => panic!("ZoruaFallible is only available for enums"),
    }

    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();

    let mut final_ts = quote! {};
    match targets {
        None => panic!("You must include at least 1 target type for the ZoruaFallible trait"),
        Some(targets) => {
            for ty in targets {
                final_ts.extend(quote! {
                    unsafe impl #impl_generics ZoruaFallible<#ty> for #ident #type_generics #where_clause {
                        fn is_valid(value: #ty) -> bool {
                            false #is_valid_impl
                        }
                    }
                    impl TryInto<#ident> for Fallible<#ident, #ty> {
                        type Error = #ty;
                        fn try_into(self) -> Result<#ident, #ty> {
                            if #ident::is_valid(self.value) {
                                Ok(unsafe { std::mem::transmute_copy(&self) })
                            } else {
                                Err(self.value)
                            }
                        }
                    }
                });
            }
        }
    }
    final_ts.into()
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
