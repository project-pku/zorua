use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::Parser, parse_str, AttrStyle, Attribute, Data, DeriveInput, Expr, Index, Lit, Type,
};

/// This derive macro works on structs and c-like enums.
///
/// Composite structs must additionally have the
/// [C repr](https://doc.rust-lang.org/nomicon/other-reprs.html#reprc),
/// in order to ensure their layout.
/// ```
/// #[derive(ZoruaField)]
/// #[repr(C)] //mandatory for composite structs
/// struct MyStruct {
///     fieldA: u16,
///     fieldB: OtherZoruaField,
/// }
/// ```
#[proc_macro_derive(ZoruaField)]
pub fn zoruafield_derive_macro(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).unwrap(); //parse
    impl_zoruafield_trait(ast) //generate
}

#[proc_macro_derive(ZoruaBitField)]
pub fn zoruabitfield_derive_macro(item: TokenStream) -> TokenStream {
    let ast = syn::parse(item).unwrap(); //parse
    impl_zoruabitfield_trait(ast) //generate
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
    match &ast.data {
        Data::Struct(data) => {
            //Ensure composite structs are repr C (otherwise size, alignment, and order are undefined)
            if (data.fields.len() > 1) && !has_repr(&ast.attrs, "C") {
                panic!(
                    "Composite structs must have the C repr to derive ZoruaField\n\
                    Try adding `#[repr(C)]` to the struct"
                )
            }

            for (i, field) in data.fields.iter().enumerate() {
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
            }
        }
        Data::Enum(data) => {
            //These two conditions ensure enum is a POD
            if !has_repr(&ast.attrs, "u8") {
                panic!(
                    "Enums must have the u8 repr to derive ZoruaField\n\
                    Try adding `#[repr(u8)]` to the struct"
                )
            }
            if data.variants.len() != 256 {
                panic!(
                    "Enums must have 256 variants to derive ZoruaField.\n\
                    Did you mean to derive ZoruaBitField or ZoruaFallible?"
                )
            }
        }
        _ => panic!("This macro is only defined for structs & enums"),
    };

    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    quote! {
        unsafe impl #impl_generics ZoruaField for #ident #type_generics #where_clause {
            fn swap_bytes_mut(&mut self) {
                #fields_impl
            }
        }
    }
    .into()
}

fn impl_zoruabitfield_trait(ast: DeriveInput) -> TokenStream {
    if !has_repr(&ast.attrs, "u8") {
        panic!("The enum must have the u8 repr, i.e. #[repr(u8)]")
    }

    let bit_repr: Type;
    match &ast.data {
        Data::Enum(data) => {
            let num_variants = data.variants.len();
            bit_repr = match num_variants {
                2 => parse_str("u1").unwrap(),
                4 => parse_str("u2").unwrap(),
                8 => parse_str("u3").unwrap(),
                16 => parse_str("u4").unwrap(),
                32 => parse_str("u5").unwrap(),
                64 => parse_str("u6").unwrap(),
                128 => parse_str("u7").unwrap(),
                256 => parse_str("u8").unwrap(),
                _ => panic!("The number of variants must equal a power of 2, from 2^1 to 2^8."),
            };

            //check if every val from 0-2^n is accounted for
            let mut exists_vec = vec![false; num_variants];
            let mut current = 0;
            for variant in &data.variants {
                if !variant.fields.is_empty() {
                    panic!("Enum must be c-like (i.e. no fields)")
                }
                match &variant.discriminant {
                    None => {
                        if current >= num_variants {
                            panic!("The disciriminants should cover every value from 0 to 2^n")
                        }
                        exists_vec[current] = true;
                        current += 1;
                    }
                    Some((_, expr)) => {
                        match expr {
                            Expr::Lit(expr) => match &expr.lit {
                                Lit::Int(lit) => {
                                    current = lit
                                        .base10_parse()
                                        .expect("Enum discriminants must be expressed in base 10");
                                    if current >= num_variants {
                                        panic!("The disciriminants should cover every value from 0 to 2^n")
                                    }
                                    exists_vec[current] = true;
                                    current += 1;
                                }
                                _ => panic!("Invalid enum discriminant!"),
                            },
                            _ => panic!("Invalid enum discriminant!"),
                        }
                    }
                }
            }
            if !exists_vec.iter().all(|value| *value) {
                panic!("The disciriminants should cover every value from 0 to 2^n")
            }
        }
        _ => panic!("This macro only supports enums"),
    }

    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    quote! {
        impl #impl_generics ZoruaBitField for #ident #type_generics #where_clause {
            type BitRepr = #bit_repr;
            fn to_bit_repr(self) -> #bit_repr {
                #bit_repr::from_backed(self as <#bit_repr as BackingBitField>::ByteRepr)
            }
            fn from_bit_repr(value: #bit_repr) -> Self {
                unsafe { std::mem::transmute(value.to_backed() as <#bit_repr as BackingBitField>::ByteRepr) }
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
