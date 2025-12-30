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
    let result = match &ast.data {
        Data::Struct(data) => impl_zoruafield_struct(&ast, data),
        Data::Enum(data) => impl_zoruafield_enum(&ast, data),
        _ => Err(syn::Error::new_spanned(
            &ast,
            "ZoruaField can only be derived for structs and enums",
        )),
    };
    result.unwrap_or_else(|e| e.into_compile_error().into())
}

fn impl_zoruafield_struct(ast: &DeriveInput, data: &DataStruct) -> Result<TokenStream, syn::Error> {
    //Ensure composite structs are repr C (otherwise size, alignment, and order are undefined)
    if (data.fields.len() > 1) && !get_repr_state(&ast.attrs)?.repr_c {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "Composite structs must have the C repr to derive ZoruaField. \
            Try adding `#[repr(C)]` to the struct.",
        ));
    }

    let no_generics = ast.generics.params.is_empty();

    let unsafe_confirm_no_padding = ast
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("unsafe_confirm_no_padding"));

    if !no_generics && !unsafe_confirm_no_padding {
        return Err(syn::Error::new_spanned(
            &ast.generics,
            "Generic structs require manual verification that no padding exists due to layout. \
            Confirm this by adding the `#[unsafe_confirm_no_padding]` attribute.",
        ));
    }

    if no_generics && unsafe_confirm_no_padding {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "The `#[unsafe_confirm_no_padding]` attribute can only be used with generic structs.",
        ));
    }

    // Generate field validation checks
    let field_checks = generate_field_checks(&data.fields);

    Ok(generate_impl(
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
    ))
}

fn impl_zoruafield_enum(ast: &DeriveInput, data: &DataEnum) -> Result<TokenStream, syn::Error> {
    //These two conditions ensure enum is a POD
    if !get_repr_state(&ast.attrs)?.repr_u8 {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "Enums must have the u8 repr to derive ZoruaField. \
            Try adding `#[repr(u8)]` to the enum.",
        ));
    }
    if data.variants.len() != 256 {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            format!(
                "Enums must have exactly 256 variants to derive ZoruaField (found {}). \
                Did you mean to derive ZoruaBitField or ZoruaFallible?",
                data.variants.len()
            ),
        ));
    }

    Ok(generate_impl("ZoruaField", true, ast, None, None))
}

#[proc_macro_derive(ZoruaBitField)]
pub fn zoruabitfield_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap(); //parse
    let result = match &ast.data {
        Data::Struct(data) => impl_zoruabitfield_struct(&ast, data),
        Data::Enum(data) => impl_zoruabitfield_enum(&ast, data),
        _ => Err(syn::Error::new_spanned(
            &ast,
            "ZoruaBitField can only be derived for enums and newtype structs",
        )),
    };
    result.unwrap_or_else(|e| e.into_compile_error().into())
}

fn impl_zoruabitfield_struct(
    ast: &DeriveInput,
    data: &DataStruct,
) -> Result<TokenStream, syn::Error> {
    if data.fields.len() != 1 {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "Only structs with exactly one field can derive ZoruaBitField",
        ));
    }
    let field0 = data.fields.iter().collect::<Vec<&syn::Field>>()[0];
    if field0.ident.is_some() {
        return Err(syn::Error::new_spanned(
            field0,
            "Only tuple structs can derive ZoruaBitField (use `struct Name(Type)` syntax)",
        ));
    }
    let wrapped_ty = &field0.ty;

    Ok(generate_impl(
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
    ))
}

fn impl_zoruabitfield_enum(ast: &DeriveInput, data: &DataEnum) -> Result<TokenStream, syn::Error> {
    if !get_repr_state(&ast.attrs)?.repr_u8 {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "The enum must have the u8 repr to derive ZoruaBitField. \
            Try adding `#[repr(u8)]` to the enum.",
        ));
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
        _ => {
            return Err(syn::Error::new_spanned(
                &ast.ident,
                format!(
                    "The number of variants must be a power of 2 from 2^1 to 2^8 (found {}).",
                    num_variants
                ),
            ));
        }
    };

    //check if every val from 0-2^n is accounted for
    let mut exists_vec = vec![false; num_variants];
    let mut current = 0;
    for variant in &data.variants {
        if !variant.fields.is_empty() {
            return Err(syn::Error::new_spanned(
                variant,
                "Enum must be c-like (variants cannot have fields)",
            ));
        }
        match &variant.discriminant {
            None => {
                if current >= num_variants {
                    return Err(syn::Error::new_spanned(
                        variant,
                        "The discriminants must cover every value from 0 to 2^n-1",
                    ));
                }
                exists_vec[current] = true;
                current += 1;
            }
            Some((_, expr)) => match expr {
                Expr::Lit(expr) => match &expr.lit {
                    Lit::Int(lit) => {
                        current = lit.base10_parse().map_err(|_| {
                            syn::Error::new_spanned(
                                lit,
                                "Enum discriminants must be expressed in base 10",
                            )
                        })?;
                        if current >= num_variants {
                            return Err(syn::Error::new_spanned(
                                variant,
                                "The discriminants must cover every value from 0 to 2^n-1",
                            ));
                        }
                        exists_vec[current] = true;
                        current += 1;
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            &expr.lit,
                            "Invalid enum discriminant: expected integer literal",
                        ));
                    }
                },
                _ => {
                    return Err(syn::Error::new_spanned(
                        expr,
                        "Invalid enum discriminant: expected literal expression",
                    ));
                }
            },
        }
    }
    if !exists_vec.iter().all(|value| *value) {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "The discriminants must cover every value from 0 to 2^n-1 without gaps",
        ));
    }

    Ok(generate_impl(
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
    ))
}

/// On top of implementing the ZoruaFallible trait, this derive macro
/// also includes a [TryInto]`<Self>` impl for [Fallible]`<Self, B>`.
#[proc_macro_derive(ZoruaFallible, attributes(target_byte, target_bit))]
pub fn zoruafallible_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap(); //parse
    let result = match &ast.data {
        Data::Enum(data) => impl_zoruafallible_enum(&ast, data),
        _ => Err(syn::Error::new_spanned(
            &ast,
            "ZoruaFallible can only be derived for enums",
        )),
    };
    result.unwrap_or_else(|e| e.into_compile_error().into())
}

/// Derive macro for newtype wrappers that can implement both `ZoruaField` and `ZoruaBitField`.
///
/// This generates conditional impls that apply based on what the wrapped type implements:
/// - `impl<T: ZoruaBitField> ZoruaBitField for Wrapper<T>` - for use in bitfields
/// - `impl<T: ZoruaField> ZoruaField for Wrapper<T>` - for use as struct fields
///
/// This allows a single generic newtype to work with:
/// - Bitfield-only types like `u4`, `u5`, etc.
/// - Field-only types like `u16_le`, `u32_le`, etc.
/// - Types that implement both like `u8`
///
/// # Requirements
/// - Must be a tuple struct with exactly one field: `struct Name<T>(pub T)`
/// - Must have `#[repr(transparent)]` to guarantee layout compatibility
///
/// # Example
/// ```ignore
/// #[derive(ZoruaNewtype)]
/// #[repr(transparent)]
/// pub struct MyId<T>(pub T);
///
/// // Now MyId<u4> works in bitfields, MyId<u16_le> works as fields,
/// // and MyId<u8> works in both contexts.
/// ```
#[proc_macro_derive(ZoruaNewtype)]
pub fn zoruanewtype_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap();
    let result = match &ast.data {
        Data::Struct(data) => impl_zoruanewtype_struct(&ast, data),
        _ => Err(syn::Error::new_spanned(
            &ast,
            "ZoruaNewtype can only be derived for newtype structs",
        )),
    };
    result.unwrap_or_else(|e| e.into_compile_error().into())
}

fn impl_zoruanewtype_struct(
    ast: &DeriveInput,
    data: &DataStruct,
) -> Result<TokenStream, syn::Error> {
    // Must be a newtype (single field)
    if data.fields.len() != 1 {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "ZoruaNewtype can only be derived for structs with exactly one field",
        ));
    }

    let field0 = data.fields.iter().next().unwrap();

    // Must be a tuple struct
    if field0.ident.is_some() {
        return Err(syn::Error::new_spanned(
            field0,
            "ZoruaNewtype can only be derived for tuple structs (use `struct Name<T>(T)` syntax)",
        ));
    }

    // Must be #[repr(transparent)]
    if !get_repr_state(&ast.attrs)?.repr_transparent {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "ZoruaNewtype requires #[repr(transparent)] to ensure layout compatibility",
        ));
    }

    let wrapped_ty = &field0.ty;
    let ident = &ast.ident;

    // Extract generic parameters without their bounds for the impl headers
    // We'll add our own bounds in the where clauses
    let generic_params: Vec<_> = ast.generics.params.iter().collect();
    let type_params: Vec<_> = ast.generics.type_params().map(|p| &p.ident).collect();

    // Build the impl generics (just the parameter names, no bounds)
    let impl_generics = if generic_params.is_empty() {
        quote! {}
    } else {
        let params = generic_params.iter().map(|p| match p {
            syn::GenericParam::Type(t) => {
                let ident = &t.ident;
                quote! { #ident }
            }
            syn::GenericParam::Lifetime(l) => quote! { #l },
            syn::GenericParam::Const(c) => {
                let ident = &c.ident;
                let ty = &c.ty;
                quote! { const #ident: #ty }
            }
        });
        quote! { <#(#params),*> }
    };

    // Build type generics (parameter names only, for the type being impl'd)
    let type_generics = if type_params.is_empty() && ast.generics.lifetimes().count() == 0 {
        quote! {}
    } else {
        let (_, ty_gen, _) = ast.generics.split_for_impl();
        quote! { #ty_gen }
    };

    // Generate both conditional impls
    let output = quote! {
        // ZoruaBitField impl - applies when wrapped type is ZoruaBitField
        impl #impl_generics ZoruaBitField for #ident #type_generics
        where #wrapped_ty: ZoruaBitField
        {
            type BitRepr = <#wrapped_ty as ZoruaBitField>::BitRepr;

            fn to_bit_repr(self) -> Self::BitRepr {
                self.0.to_bit_repr()
            }

            fn from_bit_repr(value: Self::BitRepr) -> Self {
                Self(<#wrapped_ty as ZoruaBitField>::from_bit_repr(value))
            }
        }

        // ZoruaField impl - applies when wrapped type is ZoruaField
        // SAFETY: #[repr(transparent)] guarantees this newtype has the same
        // layout as the wrapped type. If T is a valid ZoruaField (POD with
        // endian-independent representation), then this wrapper is too.
        unsafe impl #impl_generics ZoruaField for #ident #type_generics
        where #wrapped_ty: ZoruaField
        {}
    };

    Ok(output.into())
}

fn impl_zoruafallible_enum(ast: &DeriveInput, data: &DataEnum) -> Result<TokenStream, syn::Error> {
    let mut target_bytes: Vec<Type> = Vec::new();
    let mut target_bits: Vec<Type> = Vec::new();
    let repr: Type;

    for attr in &ast.attrs {
        if attr.path().is_ident("target_byte") {
            let args: proc_macro2::TokenStream = attr.parse_args().map_err(|_| {
                syn::Error::new_spanned(
                    attr,
                    "The `target_byte` attribute must contain a list of BackingField types, \
                    e.g. #[target_byte(u8, u16_le)]",
                )
            })?;
            let data = syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::parse_terminated
                .parse2(args)?;

            if !data.is_empty() {
                target_bytes.extend(data);
            }
        }

        if attr.path().is_ident("target_bit") {
            let args: proc_macro2::TokenStream = attr.parse_args().map_err(|_| {
                syn::Error::new_spanned(
                    attr,
                    "The `target_bit` attribute must contain a list of BackingBitField types, \
                    e.g. #[target_bit(u3, u7)]",
                )
            })?;
            let data = syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::parse_terminated
                .parse2(args)?;

            if !data.is_empty() {
                target_bits.extend(data);
            }
        }
    }

    let repr_state = get_repr_state(&ast.attrs)?;
    if repr_state.repr_u16 {
        repr = syn::parse_str("u16").unwrap();
    } else if repr_state.repr_u8 {
        repr = syn::parse_str("u8").unwrap();
    } else {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "The enum must have either the u8 or u16 repr. \
            Try adding `#[repr(u8)]` or `#[repr(u16)]` to the enum.",
        ));
    }

    if target_bytes.is_empty() && target_bits.is_empty() {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "You must include at least 1 target type using either #[target_byte] or #[target_bit]",
        ));
    }

    let ident = ast.ident.clone();
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    let mut final_ts = quote! {};

    // Process BackingField types (target_byte)
    for ty in target_bytes {
        // Generate variant checks for this specific type
        let mut variant_checks = quote! {};
        for variant in &data.variants {
            if !variant.fields.is_empty() {
                return Err(syn::Error::new_spanned(
                    variant,
                    "ZoruaFallible only supports c-like enums (variants cannot have fields)",
                ));
            }
            let variant_ident = &variant.ident;
            variant_checks.extend(quote! {
                if value == <#ty>::from(#ident::#variant_ident as #repr) {
                    return Ok(#ident::#variant_ident);
                }
            });
        }

        final_ts.extend(quote! {
            unsafe impl #impl_generics ZoruaFallible<#ty> for #ident #type_generics #where_clause {}

            const _: () = {
                #[inline(always)]
                fn try_from_backing(value: #ty) -> Result<#ident, #ty> {
                    #variant_checks
                    Err(value)
                }

                impl TryInto<#ident> for Fallible<#ident, #ty> {
                    type Error = #ty;
                    fn try_into(self) -> Result<#ident, #ty> {
                        try_from_backing(self.value)
                    }
                }
            };
        });
    }

    // Process BackingBitField types (target_bit)
    for ty in target_bits {
        // Generate variant checks for this specific type
        let mut variant_checks = quote! {};
        for variant in &data.variants {
            if !variant.fields.is_empty() {
                return Err(syn::Error::new_spanned(
                    variant,
                    "ZoruaFallible only supports c-like enums (variants cannot have fields)",
                ));
            }
            let variant_ident = &variant.ident;
            variant_checks.extend(quote! {
                if backed_val == (#ident::#variant_ident as <#ty as BackingBitField>::ByteRepr) {
                    return Ok(#ident::#variant_ident);
                }
            });
        }

        final_ts.extend(quote! {
            unsafe impl #impl_generics ZoruaFallible<#ty> for #ident #type_generics #where_clause {}

            const _: () = {
                #[inline(always)]
                fn try_from_backing(value: #ty) -> Result<#ident, #ty> {
                    let backed_val = <#ty as BackingBitField>::to_backed(value);
                    #variant_checks
                    Err(value)
                }

                impl TryInto<#ident> for Fallible<#ident, #ty> {
                    type Error = #ty;
                    fn try_into(self) -> Result<#ident, #ty> {
                        try_from_backing(self.value)
                    }
                }
            };
        });
    }

    Ok(final_ts.into())
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
    repr_transparent: bool,
    repr_packed: Option<usize>,
}

fn get_repr_state(attrs: &[Attribute]) -> Result<ReprState, syn::Error> {
    let mut repr_c = false;
    let mut repr_u8 = false;
    let mut repr_u16 = false;
    let mut repr_transparent = false;
    let mut repr_packed = None::<usize>;
    for attr in attrs {
        if attr.path().is_ident("repr") {
            attr.parse_nested_meta(|meta| {
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

                // #[repr(transparent)]
                if meta.path.is_ident("transparent") {
                    repr_transparent = true;
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
            })?;
        }
    }

    Ok(ReprState {
        repr_c,
        repr_u8,
        repr_u16,
        repr_transparent,
        repr_packed,
    })
}

fn get_field_types(fields: &Fields) -> impl Iterator<Item = &'_ Type> {
    fields.iter().map(|field| &field.ty)
}

fn get_fields(input: &DeriveInput) -> Result<Fields, syn::Error> {
    match &input.data {
        Data::Struct(DataStruct { fields, .. }) => Ok(fields.clone()),
        Data::Union(DataUnion { fields, .. }) => Ok(Fields::Named(fields.clone())),
        Data::Enum(_) => Err(syn::Error::new_spanned(
            input,
            "get_fields does not support enums",
        )),
    }
}

fn generate_assert_no_padding(input: &DeriveInput) -> proc_macro2::TokenStream {
    let struct_type = &input.ident;
    let span = input.ident.span();
    // Note: get_fields won't fail here since we only call this for structs
    let fields = get_fields(input).unwrap();

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

// ============================================================================
// bitfields! proc macro for struct transformation
// ============================================================================

use syn::{
    Ident, Token, Visibility, braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

/// A field in the zorua struct, which may have native conversion syntax
struct ZoruaField {
    attrs: Vec<Attribute>,
    vis: Visibility,
    name: Ident,
    native_type: Type,
    storage_type: Type, // Same as native_type if no `as` clause
    has_backing_type: bool,
    bitfield_subfields: Option<Vec<BitfieldSubfield>>,
}

struct BitfieldSubfield {
    attrs: Vec<Attribute>,
    vis: Visibility,
    name: Ident,
    ty: proc_macro2::TokenStream,
    bit_offset: LitInt,
}

impl Parse for ZoruaField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;

        // Parse the native type by collecting tokens until we hit `as`, `{`, or end
        // We can't use Type::parse() because it doesn't handle `as` keyword properly
        let mut native_type_tokens = proc_macro2::TokenStream::new();
        let mut depth = 0; // Track <> nesting

        loop {
            if input.is_empty() {
                break;
            }

            // Check for terminators (at depth 0)
            if depth == 0 {
                if input.peek(Token![as]) || input.peek(syn::token::Brace) || input.peek(Token![,])
                {
                    break;
                }
            }

            // Track <> nesting for generics like Fallible<Language, u8>
            if input.peek(Token![<]) {
                depth += 1;
            } else if input.peek(Token![>]) {
                depth -= 1;
            }

            // Consume next token
            let tt: proc_macro2::TokenTree = input.parse()?;
            native_type_tokens.extend(std::iter::once(tt));
        }

        let native_type: Type = syn::parse2(native_type_tokens.clone()).map_err(|e| {
            syn::Error::new(name.span(), format!("Failed to parse native type: {}", e))
        })?;

        // Check for `as StorageType`
        let (storage_type, has_backing_type) = if input.peek(Token![as]) {
            input.parse::<Token![as]>()?;
            let storage: Type = input.parse()?;
            (storage, true)
        } else {
            (native_type.clone(), false)
        };

        // Check for bitfield subfields { ... }
        let bitfield_subfields = if input.peek(syn::token::Brace) {
            let content;
            braced!(content in input);
            let subfields: Punctuated<BitfieldSubfield, Token![,]> =
                content.parse_terminated(BitfieldSubfield::parse, Token![,])?;
            Some(subfields.into_iter().collect())
        } else {
            None
        };

        Ok(ZoruaField {
            attrs,
            vis,
            name,
            native_type,
            storage_type,
            has_backing_type,
            bitfield_subfields,
        })
    }
}

impl Parse for BitfieldSubfield {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;

        // Parse the type - collect tokens until we hit @
        let mut ty_tokens = proc_macro2::TokenStream::new();
        let mut depth = 0; // Track <> nesting

        loop {
            if input.is_empty() {
                return Err(syn::Error::new(name.span(), "Expected @ after type"));
            }

            // Check for @ at depth 0
            if depth == 0 && input.peek(Token![@]) {
                break;
            }

            // Track <> nesting for generics
            if input.peek(Token![<]) {
                depth += 1;
            } else if input.peek(Token![>]) {
                depth -= 1;
            }

            // Consume next token
            let tt: proc_macro2::TokenTree = input.parse()?;
            ty_tokens.extend(std::iter::once(tt));
        }

        input.parse::<Token![@]>()?;
        let bit_offset: LitInt = input.parse()?;

        Ok(BitfieldSubfield {
            attrs,
            vis,
            name,
            ty: ty_tokens,
            bit_offset,
        })
    }
}

struct ZoruaStruct {
    attrs: Vec<Attribute>,
    vis: Visibility,
    name: Ident,
    generics: syn::Generics,
    fields: Vec<ZoruaField>,
}

impl Parse for ZoruaStruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        input.parse::<Token![struct]>()?;
        let name: Ident = input.parse()?;
        let generics: syn::Generics = input.parse()?;

        let content;
        braced!(content in input);
        let fields: Punctuated<ZoruaField, Token![,]> =
            content.parse_terminated(ZoruaField::parse, Token![,])?;

        Ok(ZoruaStruct {
            attrs,
            vis,
            name,
            generics,
            fields: fields.into_iter().collect(),
        })
    }
}

/// The `bitfields!` proc macro transforms a struct with zorua field syntax.
///
/// # Field Syntax
///
/// - Regular field: `pub field: Type`
///
/// - Bitfield container: `field: Type { subfield: SubType@bit_offset, ... }`
///
/// # Example
///
/// ```ignore
/// bitfields! {
///     #[repr(C)]
///     #[derive(ZoruaField, Clone, Debug)]
///     pub struct Pokemon {
///         pub pid: u32_le,
///         pub tid: u32_le,
///         flags: u8 {
///             pub is_egg: bool@0,
///             pub has_nickname: bool@1,
///         },
///     }
/// }
/// ```
#[proc_macro]
pub fn bitfields(item: TokenStream) -> TokenStream {
    let input = match syn::parse::<ZoruaStruct>(item) {
        Ok(parsed) => parsed,
        Err(e) => {
            return e.into_compile_error().into();
        }
    };

    match generate_zorua_struct(input) {
        Ok(tokens) => tokens,
        Err(e) => e.into_compile_error().into(),
    }
}

fn generate_zorua_struct(input: ZoruaStruct) -> Result<TokenStream, syn::Error> {
    let ZoruaStruct {
        attrs,
        vis,
        name,
        generics,
        fields,
    } = input;

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Generate field definitions with _raw suffix for backed fields
    let field_defs: Vec<_> = fields
        .iter()
        .map(|f| {
            let field_attrs = &f.attrs;
            let field_vis = &f.vis;
            let field_name = if f.has_backing_type {
                syn::Ident::new(&format!("{}_raw", f.name), f.name.span())
            } else {
                f.name.clone()
            };
            let storage_type = &f.storage_type;
            quote! {
                #(#field_attrs)*
                #field_vis #field_name: #storage_type
            }
        })
        .collect();

    // Generate field accessors (getter and setter) ONLY for backed fields (those with `as BackingType`)
    // Regular fields (no `as`) just get direct access
    let accessors: Vec<_> = fields.iter().filter(|f| f.has_backing_type).map(|f| {
        let field_attrs = &f.attrs;
        let field_vis = &f.vis;
        let getter_name = &f.name;
        let setter_name = syn::Ident::new(&format!("set_{}", f.name), f.name.span());
        let raw_name = syn::Ident::new(&format!("{}_raw", f.name), f.name.span());
        let native_type = &f.native_type;
        let storage_type = &f.storage_type;

        // Check if both types are arrays - if so, generate element-wise conversion
        if let (Type::Array(native_arr), Type::Array(storage_arr)) = (native_type, storage_type) {
            let native_elem = &native_arr.elem;
            let storage_elem = &storage_arr.elem;

            // Non-fallible array field: getter returns array with element-wise conversion
            quote! {
                #(#field_attrs)*
                #[doc = concat!("Get the [`", stringify!(#native_type), "`] value from the `", stringify!(#raw_name), "` field.")]
                #field_vis fn #getter_name(&self) -> #native_type {
                    self.#raw_name.clone().map(|s| <#native_elem as ZoruaNative<#storage_elem>>::from_storage(s))
                }

                #(#field_attrs)*
                #[doc = concat!("Set the `", stringify!(#raw_name), "` field from a [`", stringify!(#native_type), "`] value.")]
                #field_vis fn #setter_name(&mut self, val: #native_type) {
                    self.#raw_name = val.map(|n| <#native_elem as ZoruaNative<#storage_elem>>::to_storage(n));
                }
            }
        } else {
            // Non-fallible field: getter returns NativeType directly (via ZoruaNative)
            quote! {
                #(#field_attrs)*
                #[doc = concat!("Get the [`", stringify!(#native_type), "`] value from the `", stringify!(#raw_name), "` field.")]
                #field_vis fn #getter_name(&self) -> #native_type {
                    <#native_type as ZoruaNative<#storage_type>>::from_storage(self.#raw_name.clone())
                }

                #(#field_attrs)*
                #[doc = concat!("Set the `", stringify!(#raw_name), "` field from a [`", stringify!(#native_type), "`] value.")]
                #field_vis fn #setter_name(&mut self, val: #native_type) {
                    self.#raw_name = <#native_type as ZoruaNative<#storage_type>>::to_storage(val);
                }
            }
        }
    }).collect();

    // Generate bitfield subfield accessors
    let bitfield_accessors: Vec<_> = fields.iter().filter_map(|f| {
        f.bitfield_subfields.as_ref().map(|subfields| {
            let container_raw_name = if f.has_backing_type {
                syn::Ident::new(&format!("{}_raw", f.name), f.name.span())
            } else {
                f.name.clone()
            };

            subfields.iter().map(|sf| {
                let sf_attrs = &sf.attrs;
                let sf_vis = &sf.vis;
                let sf_name = &sf.name;
                let sf_setter = syn::Ident::new(&format!("set_{}", sf.name), sf.name.span());
                let sf_type = &sf.ty;
                let sf_offset = &sf.bit_offset;

                // Parse type to check if it's an array [T; N]
                let type_result: Result<Type, _> = syn::parse2(sf_type.clone());

                if let Ok(Type::Array(type_array)) = type_result {
                    let elem = &type_array.elem;
                    // Array bitfield: generate indexed accessors
                    quote! {
                        #(#sf_attrs)*
                        #sf_vis fn #sf_name(&self, index: usize) -> #elem {
                            let bit_len = <#elem as ZoruaBitField>::BitRepr::BITS as usize;
                            let offset = #sf_offset + (bit_len * index);
                            let bit_repr = self.#container_raw_name.get_bits_at::<<#elem as ZoruaBitField>::BitRepr>(offset);
                            <#elem as ZoruaBitField>::from_bit_repr(bit_repr)
                        }

                        #(#sf_attrs)*
                        #sf_vis fn #sf_setter(&mut self, val: #elem, index: usize) {
                            let bit_repr = val.to_bit_repr();
                            let bit_len = <#elem as ZoruaBitField>::BitRepr::BITS as usize;
                            let offset = #sf_offset + (bit_len * index);
                            self.#container_raw_name.set_bits_at::<<#elem as ZoruaBitField>::BitRepr>(bit_repr, offset);
                        }
                    }
                } else {
                    // Non-array bitfield: generate simple value accessors
                    quote! {
                        #(#sf_attrs)*
                        #sf_vis fn #sf_name(&self) -> #sf_type {
                            let bit_repr = self.#container_raw_name.get_bits_at::<<#sf_type as ZoruaBitField>::BitRepr>(#sf_offset);
                            <#sf_type as ZoruaBitField>::from_bit_repr(bit_repr)
                        }

                        #(#sf_attrs)*
                        #sf_vis fn #sf_setter(&mut self, val: #sf_type) {
                            let bit_repr = val.to_bit_repr();
                            self.#container_raw_name.set_bits_at::<<#sf_type as ZoruaBitField>::BitRepr>(bit_repr, #sf_offset);
                        }
                    }
                }
            }).collect::<Vec<_>>()
        })
    }).flatten().collect();

    let output = quote! {
        #(#attrs)*
        #vis struct #name #generics {
            #(#field_defs),*
        }

        impl #impl_generics #name #ty_generics #where_clause {
            #(#accessors)*
            #(#bitfield_accessors)*
        }
    };

    Ok(output.into())
}
