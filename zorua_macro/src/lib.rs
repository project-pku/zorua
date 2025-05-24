use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    Attribute, Data, DataEnum, DataStruct, DataUnion, DeriveInput, Expr, Fields, Lit, LitInt, Type,
    parse::Parser, parse_str, token,
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
#[proc_macro_derive(ZoruaField, attributes(unsafe_confirm_no_padding))]
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
    if (data.fields.len() > 1) && !get_repr_state(&ast.attrs).repr_c {
        panic!(
            "Composite structs must have the C repr to derive ZoruaField\n\
            Try adding `#[repr(C)]` to the struct"
        )
    }

    let no_generics = ast.generics.params.is_empty();

    let unsafe_confirm_no_padding = ast
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("unsafe_confirm_no_padding"));

    if !no_generics && !unsafe_confirm_no_padding {
        panic!(
            "You must manually verify that the generic structs has no padding due to its layout. You can confirm this by adding the `unsafe_no_padding` attribute."
        )
    }

    if no_generics && unsafe_confirm_no_padding {
        panic!("The `unsafe_no_padding` attribute can only be used with generic structs.")
    }

    // Generate field validation checks
    let field_checks = generate_field_checks(&data.fields);

    generate_impl(
        "ZoruaField",
        true,
        ast,
        None,
        //NOTE: Allow packed structs to skip the padding check?
        if no_generics {
            let padding_check = generate_assert_no_padding(ast);
            Some(quote! {
                #field_checks
                #padding_check
            })
        } else {
            Some(field_checks)
        },
    )
}

fn impl_zoruafield_enum(ast: &DeriveInput, data: &DataEnum) -> TokenStream {
    //These two conditions ensure enum is a POD
    if !get_repr_state(&ast.attrs).repr_u8 {
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

    generate_impl("ZoruaField", true, ast, None, None)
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
        Some(quote! {
            type BitRepr = <#wrapped_ty as ZoruaBitField>::BitRepr;
            fn to_bit_repr(self) -> Self::BitRepr {
                self.0.to_bit_repr()
            }
            fn from_bit_repr(value: Self::BitRepr) -> Self {
                Self(<#wrapped_ty as ZoruaBitField>::from_bit_repr(value))
            }
        }),
        None,
    )
}

fn impl_zoruabitfield_enum(ast: &DeriveInput, data: &DataEnum) -> TokenStream {
    if !get_repr_state(&ast.attrs).repr_u8 {
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
        Some(quote! {
            type BitRepr = #bit_repr;
            fn to_bit_repr(self) -> Self::BitRepr {
                Self::BitRepr::from_backed(self as <Self::BitRepr as BackingBitField>::ByteRepr)
            }
            fn from_bit_repr(value: Self::BitRepr) -> Self {
                // SAFETY:
                // 1) Every possible value of BitRepr corresponds to a variant (as verified above)
                // 2) alignment is not a concern for transmuting values (as opposed to references)
                unsafe { core::mem::transmute(value.to_backed() as <Self::BitRepr as BackingBitField>::ByteRepr) }
            }
        }),
        None,
    )
}

/// On top of implementing the ZoruaFallible trait, this derive macro
/// also includes a [TryInto]`<Self>` impl for [Fallible]`<Self, B>`.
#[proc_macro_derive(ZoruaFallible, attributes(target_byte, target_bit))]
pub fn zoruafallible_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap(); //parse
    match &ast.data {
        Data::Enum(data) => impl_zoruafallible_enum(&ast, data),
        _ => panic!("ZoruaFallible is only available for enums"),
    } //generate
}

fn impl_zoruafallible_enum(ast: &DeriveInput, data: &DataEnum) -> TokenStream {
    let mut target_bytes: Vec<Type> = Vec::new();
    let mut target_bits: Vec<Type> = Vec::new();
    let repr: Type;

    for attr in &ast.attrs {
        if attr.path().is_ident("target_byte") {
            let data = syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::parse_terminated
                .parse2(attr.parse_args().expect(
                    "The `target_byte` attribute must contain a list of BackingField types, e.g. #[target_byte(u8, u16_le)]",
                ))
                .unwrap();

            if !data.is_empty() {
                target_bytes.extend(data);
            }
        }

        if attr.path().is_ident("target_bit") {
            let data = syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::parse_terminated
                .parse2(attr.parse_args().expect(
                    "The `target_bit` attribute must contain a list of BackingBitField types, e.g. #[target_bit(u3, u7)]",
                ))
                .unwrap();

            if !data.is_empty() {
                target_bits.extend(data);
            }
        }
    }

    let repr_state = get_repr_state(&ast.attrs);
    if repr_state.repr_u16 {
        repr = syn::parse_str("u16").unwrap();
    } else if repr_state.repr_u8 {
        repr = syn::parse_str("u8").unwrap();
    } else {
        panic!("The enum must have either the u8 or u16 repr")
    }

    if target_bytes.is_empty() && target_bits.is_empty() {
        panic!(
            "You must include at least 1 target type using either #[target_byte] or #[target_bit]"
        );
    }

    let ident = ast.ident.clone();
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    let mut final_ts = quote! {};

    // Process BackingField types (target_byte)
    for (idx, ty) in target_bytes.into_iter().enumerate() {
        // Generate variant checks for this specific type
        let mut variant_checks = quote! {};
        for variant in &data.variants {
            if !variant.fields.is_empty() {
                panic!("ZoruaFallible only supports c-like enums (i.e. no fields).")
            }
            let variant_ident = &variant.ident;
            variant_checks.extend(quote! {
                if value == <#ty>::from(#ident::#variant_ident as #repr) {
                    return Ok(#ident::#variant_ident);
                }
            });
        }

        // Create a type-specific helper function
        let helper_fn_name = syn::Ident::new(
            &format!(
                "__zorua_try_from_byte_{}_for_{}",
                idx,
                ident.to_string().to_lowercase()
            ),
            ident.span(),
        );

        final_ts.extend(quote! {
            #[doc(hidden)]
            #[inline(always)]
            fn #helper_fn_name(value: #ty) -> Result<#ident, #ty> {
                #variant_checks
                Err(value)
            }

            unsafe impl #impl_generics ZoruaFallible<#ty> for #ident #type_generics #where_clause {}

            impl TryInto<#ident> for Fallible<#ident, #ty> {
                type Error = #ty;
                fn try_into(self) -> Result<#ident, #ty> {
                    #helper_fn_name(self.value)
                }
            }
        });
    }

    // Process BackingBitField types (target_bit)
    for (idx, ty) in target_bits.into_iter().enumerate() {
        // Generate variant checks for this specific type
        let mut variant_checks = quote! {};
        for variant in &data.variants {
            if !variant.fields.is_empty() {
                panic!("ZoruaFallible only supports c-like enums (i.e. no fields).")
            }
            let variant_ident = &variant.ident;
            variant_checks.extend(quote! {
                if backed_val == (#ident::#variant_ident as <#ty as BackingBitField>::ByteRepr) {
                    return Ok(#ident::#variant_ident);
                }
            });
        }

        // Create a type-specific helper function
        let helper_fn_name = syn::Ident::new(
            &format!(
                "__zorua_try_from_bit_{}_for_{}",
                idx,
                ident.to_string().to_lowercase()
            ),
            ident.span(),
        );

        final_ts.extend(quote! {
            #[doc(hidden)]
            #[inline(always)]
            fn #helper_fn_name(value: #ty) -> Result<#ident, #ty> {
                let backed_val = <#ty as BackingBitField>::to_backed(value);
                #variant_checks
                Err(value)
            }

            unsafe impl #impl_generics ZoruaFallible<#ty> for #ident #type_generics #where_clause {}

            impl TryInto<#ident> for Fallible<#ident, #ty> {
                type Error = #ty;
                fn try_into(self) -> Result<#ident, #ty> {
                    #helper_fn_name(self.value)
                }
            }
        });
    }

    final_ts.into()
}

// --------------
// Helper fns
// --------------
fn generate_impl(
    ty: &str,
    is_unsafe: bool,
    ast: &DeriveInput,
    tokens: Option<proc_macro2::TokenStream>,
    asserts: Option<proc_macro2::TokenStream>,
) -> TokenStream {
    let ty: Type = syn::parse_str(ty).unwrap();
    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    let unsafe_keyword = is_unsafe.then_some(quote! {unsafe});
    quote! {
        #unsafe_keyword impl #impl_generics #ty for #ident #type_generics #where_clause {
            #tokens
        }

        impl #impl_generics #ident #type_generics #where_clause {
            #asserts
        }
    }
    .into()
}

#[allow(unused)]
struct ReprState {
    repr_c: bool,
    repr_u8: bool,
    repr_u16: bool,
    repr_packed: Option<usize>,
}

fn get_repr_state(attrs: &[Attribute]) -> ReprState {
    let mut repr_c = false;
    let mut repr_u8 = false;
    let mut repr_u16 = false;
    let mut repr_packed = None::<usize>;
    for attr in attrs {
        if attr.path().is_ident("repr") {
            let res = attr.parse_nested_meta(|meta| {
                // #[repr(C)]
                if meta.path.is_ident("C") {
                    repr_c = true;
                    return Ok(());
                }

                // #[repr(u8)]
                if meta.path.is_ident("u8") {
                    repr_u8 = true;
                    return Ok(());
                }

                // #[repr(u16)]
                if meta.path.is_ident("u16") {
                    repr_u16 = true;
                    return Ok(());
                }

                // #[repr(packed)] or #[repr(packed(N))], omitted N means 1
                if meta.path.is_ident("packed") {
                    if meta.input.peek(token::Paren) {
                        let content;
                        syn::parenthesized!(content in meta.input);
                        let lit: LitInt = content.parse()?;
                        let n: usize = lit.base10_parse()?;
                        repr_packed = Some(n);
                    } else {
                        repr_packed = Some(1);
                    }
                    return Ok(());
                }

                Ok(()) //ignore unparsed reprs
            });
            if let Err(e) = res {
                panic!("zorua crate failed to parse repr attribute: {e}");
            }
        }
    }

    ReprState {
        repr_c,
        repr_u8,
        repr_u16,
        repr_packed,
    }
}

fn get_field_types(fields: &Fields) -> impl Iterator<Item = &'_ Type> {
    fields.iter().map(|field| &field.ty)
}

fn get_fields(input: &DeriveInput) -> Fields {
    match &input.data {
        Data::Struct(DataStruct { fields, .. }) => fields.clone(),
        Data::Union(DataUnion { fields, .. }) => Fields::Named(fields.clone()),
        Data::Enum(_) => panic!("Enums are not supported"),
    }
}

fn generate_assert_no_padding(input: &DeriveInput) -> proc_macro2::TokenStream {
    let struct_type = &input.ident;
    let span = input.ident.span();
    let fields = get_fields(input);

    let mut field_types = get_field_types(&fields);
    let size_sum = if let Some(first) = field_types.next() {
        let size_first = quote_spanned!(span => ::core::mem::size_of::<#first>());
        let size_rest = quote_spanned!(span => #( + ::core::mem::size_of::<#field_types>() )*);
        quote_spanned!(span => #size_first #size_rest)
    } else {
        quote_spanned!(span => 0)
    };

    //NOTE: This works but does not provide a very helpful error message...
    quote! {
        #[doc(hidden)]
        const _ZORUA_PADDING_CHECK: fn() = || {
            let _ = ::core::mem::transmute::<#struct_type, [u8; #size_sum]>;
        };
    }
}

fn generate_field_checks(fields: &Fields) -> proc_macro2::TokenStream {
    let mut field_checks = quote! {};

    for (i, field) in fields.iter().enumerate() {
        let ty = &field.ty;
        let const_name = syn::Ident::new(
            &format!("_ZORUA_FIELD_CHECK_{i}"),
            proc_macro2::Span::call_site(),
        );

        // Generate a compile-time assertion that the field implements ZoruaField
        field_checks.extend(quote! {
            #[doc(hidden)]
            const #const_name: fn() = || {
                // This will fail to compile if the field type doesn't implement ZoruaField
                fn assert_zorua_field<T: ZoruaField>() {}
                assert_zorua_field::<#ty>();
            };
        });
    }

    field_checks
}
