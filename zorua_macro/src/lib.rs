use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::Parser, parse_str, AttrStyle, Attribute, Data, DataEnum, DataStruct, DeriveInput, Expr,
    Index, Lit, Type,
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
    let ast: DeriveInput = syn::parse(item).unwrap(); //parse
    match &ast.data {
        Data::Struct(data) => impl_zoruafield_struct(&ast, data),
        Data::Enum(data) => impl_zoruafield_enum(&ast, data),
        _ => panic!("This macro is only defined for structs & enums"),
    } //generate
}

fn impl_zoruafield_struct(ast: &DeriveInput, data: &DataStruct) -> TokenStream {
    //Ensure composite structs are repr C (otherwise size, alignment, and order are undefined)
    if (data.fields.len() > 1) && !has_repr(&ast.attrs, "C") {
        panic!(
            "Composite structs must have the C repr to derive ZoruaField\n\
            Try adding `#[repr(C)]` to the struct"
        )
    }

    let mut fields_impl = quote! {};
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

    generate_impl(
        "ZoruaField",
        true,
        ast,
        quote! {
            fn swap_bytes_mut(&mut self) {
                #fields_impl
            }
        },
    )
}

fn impl_zoruafield_enum(ast: &DeriveInput, data: &DataEnum) -> TokenStream {
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

    generate_impl(
        "ZoruaField",
        true,
        ast,
        quote! {
            fn swap_bytes_mut(&mut self) {}
        },
    )
}

#[proc_macro_derive(ZoruaBitField)]
pub fn zoruabitfield_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap(); //parse
    match &ast.data {
        Data::Struct(data) => impl_zoruabitfield_struct(&ast, data),
        Data::Enum(data) => impl_zoruabitfield_enum(&ast, data),
        _ => panic!("This macro only supports enums and newtype structs"),
    } //generate
}

fn impl_zoruabitfield_struct(ast: &DeriveInput, data: &DataStruct) -> TokenStream {
    if data.fields.len() != 1 {
        panic!("Only structs with one field can derive ZoruaBitField")
    }
    let field0 = data.fields.iter().collect::<Vec<&syn::Field>>()[0];
    if field0.ident.is_some() {
        panic!("Only tuple structs can derive ZoruaBitField")
    }
    let wrapped_ty = &field0.ty;

    generate_impl(
        "ZoruaBitField",
        false,
        ast,
        quote! {
            type BitRepr = <#wrapped_ty as ZoruaBitField>::BitRepr;
            fn to_bit_repr(self) -> Self::BitRepr {
                self.0.to_bit_repr()
            }
            fn from_bit_repr(value: Self::BitRepr) -> Self {
                Self(<#wrapped_ty as ZoruaBitField>::from_bit_repr(value))
            }
        },
    )
}

fn impl_zoruabitfield_enum(ast: &DeriveInput, data: &DataEnum) -> TokenStream {
    if !has_repr(&ast.attrs, "u8") {
        panic!("The enum must have the u8 repr, i.e. #[repr(u8)]")
    }

    let num_variants = data.variants.len();
    let bit_repr: Type = match num_variants {
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
                    panic!("The discriminants should cover every value from 0 to 2^n-1")
                }
                exists_vec[current] = true;
                current += 1;
            }
            Some((_, expr)) => match expr {
                Expr::Lit(expr) => match &expr.lit {
                    Lit::Int(lit) => {
                        current = lit
                            .base10_parse()
                            .expect("Enum discriminants must be expressed in base 10");
                        if current >= num_variants {
                            panic!("The discriminants should cover every value from 0 to 2^n-1")
                        }
                        exists_vec[current] = true;
                        current += 1;
                    }
                    _ => panic!("Invalid enum discriminant!"),
                },
                _ => panic!("Invalid enum discriminant!"),
            },
        }
    }
    if !exists_vec.iter().all(|value| *value) {
        panic!("The discriminants should cover every value from 0 to 2^n-1")
    }

    generate_impl(
        "ZoruaBitField",
        false,
        ast,
        quote! {
            type BitRepr = #bit_repr;
            fn to_bit_repr(self) -> Self::BitRepr {
                Self::BitRepr::from_backed(self as <Self::BitRepr as BackingBitField>::ByteRepr)
            }
            fn from_bit_repr(value: Self::BitRepr) -> Self {
                // SAFETY:
                // 1) Every possible value of BitRepr corresponds to a variant (as verified above)
                // 2) alignment is not a concern for transmuting values (as opposed to references)
                unsafe { std::mem::transmute(value.to_backed() as <Self::BitRepr as BackingBitField>::ByteRepr) }
            }
        },
    )
}

/// On top of implementing the ZoruaFallible trait, this derive macro
/// also includes a [TryInto]`<Self>` impl for [Fallible]`<Self, B>`.
#[proc_macro_derive(ZoruaFallible, attributes(targets))]
pub fn zoruafallible_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap(); //parse
    match &ast.data {
        Data::Enum(data) => impl_zoruafallible_enum(&ast, data),
        _ => panic!("ZoruaFallible is only available for enums"),
    } //generate
}

fn impl_zoruafallible_enum(ast: &DeriveInput, data: &DataEnum) -> TokenStream {
    let mut targets: Option<Vec<Type>> = None;
    let repr: Type;

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

    if has_repr(&ast.attrs, "u8") {
        repr = syn::parse_str("u8").unwrap();
    } else if has_repr(&ast.attrs, "u16") {
        repr = syn::parse_str("u16").unwrap();
    } else {
        panic!("The enum must have either the u8 or u16 repr")
    }

    match targets {
        None => panic!("You must include at least 1 target type for the ZoruaFallible trait"),
        Some(targets) => {
            let ident = ast.ident.clone();
            let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
            let mut final_ts = quote! {};

            for ty in targets {
                //generate is_valid || arms
                let mut is_valid_impl = quote! {};
                for variant in &data.variants {
                    if !variant.fields.is_empty() {
                        panic!("ZoruaFallible only supports c-like enums (i.e. no fields).")
                    }
                    let variant_ident = &variant.ident;
                    is_valid_impl.extend(quote! {
                        || (backed_val == #ident::#variant_ident as <#ty as BackingBitField>::ByteRepr)
                    });
                }

                final_ts.extend(quote! {
                    unsafe impl #impl_generics ZoruaFallible<#ty> for #ident #type_generics #where_clause {
                        fn is_valid(value: #ty) -> bool {
                            let backed_val = <#ty as BackingBitField>::to_backed(value);
                            false #is_valid_impl
                        }
                    }
                    impl TryInto<#ident> for Fallible<#ident, #ty> {
                        type Error = #ty;
                        fn try_into(self) -> Result<#ident, #ty> {
                            if #ident::is_valid(self.value) {
                                let backed_value = <#ty as BackingBitField>::to_backed(self.value);
                                // SAFETY:
                                // 1) We have already ensured this is a c-like enum
                                // 2) We just checked that this is a valid value
                                // 3) The `as #repr` ensures that the backed_value is
                                // same shape as the discriminant
                                Ok(unsafe { std::mem::transmute(backed_value as #repr) })
                            } else {
                                Err(self.value)
                            }
                        }
                    }
                });
            }
            final_ts.into()
        }
    }
}

// --------------
// Helper fns
// --------------
fn generate_impl(
    ty: &str,
    is_unsafe: bool,
    ast: &DeriveInput,
    tokens: proc_macro2::TokenStream,
) -> TokenStream {
    let ty: Type = syn::parse_str(ty).unwrap();
    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    let unsafe_keyword = is_unsafe.then_some(quote! {unsafe});
    quote! {
        #unsafe_keyword impl #impl_generics #ty for #ident #type_generics #where_clause {
            #tokens
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
        let attr = attr.to_token_stream().to_string();
        if !attr.contains("#[repr(") || !attr.contains(repr) {
            continue;
        }

        return true;
    }
    false
}
