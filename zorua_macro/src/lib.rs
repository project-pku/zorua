use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, LitInt, Type, token,
};

/// This derive macro works on structs and c-like enums.
///
/// Composite structs must additionally have the
/// [C repr](https://doc.rust-lang.org/nomicon/other-reprs.html#reprc),
/// in order to ensure their layout.
/// ```ignore
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

    Ok(generate_impl(
        "ZoruaField",
        true,
        ast,
        None,
        None,
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
                "Enums must have exactly 256 variants to derive ZoruaField (found {}).",
                data.variants.len()
            ),
        ));
    }

    Ok(generate_impl("ZoruaField", true, ast, None, None))
}

/// Derive macro that implements `ZoruaNative` trait for enums and newtype structs.
///
/// For enums with `#[repr(u8)]` or `#[repr(u16)]`, implements `ZoruaNative<ReprType>`
/// where the conversion validates the discriminant value.
///
/// For newtype structs with a single field, delegates to the inner type's `ZoruaNative` implementation.
///
/// # Requirements for enums
/// - Must have `#[repr(u8)]` or `#[repr(u16)]`
/// - All variants must have explicit discriminant values
///
/// # Requirements for newtype structs  
/// - Must be a tuple struct with exactly one field
/// - Must have `#[repr(transparent)]`
///
/// # Example
/// ```ignore
/// #[derive(ZoruaNative)]
/// #[repr(u8)]
/// pub enum Language {
///     Japanese = 1,
///     English = 2,
///     French = 3,
/// }
///
/// // Now Language implements ZoruaNative<u8>
/// ```
#[proc_macro_derive(ZoruaNative)]
pub fn zoruanative_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap();
    let result = match &ast.data {
        Data::Enum(data) => impl_zoruanative_enum(&ast, data),
        Data::Struct(data) => impl_zoruanative_struct(&ast, data),
        _ => Err(syn::Error::new_spanned(
            &ast,
            "ZoruaNative can only be derived for enums and structs",
        )),
    };
    result.unwrap_or_else(|e| e.into_compile_error().into())
}

fn impl_zoruanative_enum(ast: &DeriveInput, data: &DataEnum) -> Result<TokenStream, syn::Error> {
    let repr_state = get_repr_state(&ast.attrs)?;

    // Determine cast repr type and max bits (used for self as Repr)
    let (cast_repr, max_bits): (Type, usize) = if repr_state.repr_u8 {
        (syn::parse_str("u8").unwrap(), 8)
    } else if repr_state.repr_u16 {
        (syn::parse_str("u16").unwrap(), 16)
    } else {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "ZoruaNative requires #[repr(u8)] or #[repr(u16)] for enums",
        ));
    };

    let ident = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();

    // Validate variants and build match arms
    let mut match_arms = quote! {};
    for variant in &data.variants {
        if !variant.fields.is_empty() {
            return Err(syn::Error::new_spanned(
                variant,
                "ZoruaNative only supports c-like enums (variants cannot have fields)",
            ));
        }
        if variant.discriminant.is_none() {
            return Err(syn::Error::new_spanned(
                variant,
                "ZoruaNative requires explicit discriminant values for all variants",
            ));
        }
        let variant_ident = &variant.ident;
        match_arms.extend(quote! {
            x if x == #ident::#variant_ident as #cast_repr => Ok(#ident::#variant_ident),
        });
    }

    // Compute minimum bits required: ceil(log2(n)) for n variants
    let variant_count = data.variants.len();
    let min_bits = if variant_count <= 1 {
        1
    } else {
        (usize::BITS - (variant_count - 1).leading_zeros()) as usize
    };

    // Build list of all storage types to implement for:
    // - All uX types from min_bits to max_bits (matching repr)
    // - For u16+, also add _le and _be endian variants
    let mut all_types: Vec<(Type, bool, usize)> = Vec::new(); // (type, is_endian, bits)

    // Add ux2 types for bits < 8 (u1-u7)
    for bits in min_bits..8.min(max_bits + 1) {
        let ty_name = format!("u{}", bits);
        if let Ok(ty) = syn::parse_str::<Type>(&ty_name) {
            all_types.push((ty, false, bits));
        }
    }

    // Add u8 if we have repr(u8) or repr(u16)
    if min_bits <= 8 && max_bits >= 8 {
        all_types.push((syn::parse_str("u8").unwrap(), false, 8));
    }

    // Add ux2 types for bits 9-15 if repr(u16)
    if max_bits >= 16 {
        for bits in 9.max(min_bits)..16 {
            let ty_name = format!("u{}", bits);
            if let Ok(ty) = syn::parse_str::<Type>(&ty_name) {
                all_types.push((ty, false, bits));
            }
        }
    }

    // Add u16 and endian variants if repr(u16)
    if min_bits <= 16 && max_bits >= 16 {
        all_types.push((syn::parse_str("u16").unwrap(), false, 16));
        all_types.push((syn::parse_str("u16_le").unwrap(), true, 16));
        all_types.push((syn::parse_str("u16_be").unwrap(), true, 16));
    }

    // Also add endian variants for larger types when using native primitives
    // u32_le/be and u64_le/be - these allow enums to be used where the storage
    // is wider than needed (like padding fields)
    if max_bits >= 8 {
        // These are always fallible since variant_count << 2^32 or 2^64
        all_types.push((syn::parse_str("u32").unwrap(), false, 32));
        all_types.push((syn::parse_str("u32_le").unwrap(), true, 32));
        all_types.push((syn::parse_str("u32_be").unwrap(), true, 32));
        all_types.push((syn::parse_str("u64").unwrap(), false, 64));
        all_types.push((syn::parse_str("u64_le").unwrap(), true, 64));
        all_types.push((syn::parse_str("u64_be").unwrap(), true, 64));
    }

    let mut impls = quote! {};

    for (target_ty, is_endian, bits) in all_types {
        // Determine if this impl is fallible:
        // Fallible when variant_count < 2^bits (not all bit patterns are valid)
        let is_fallible = if bits >= 64 {
            true // Always fallible for 64-bit (can't have 2^64 variants)
        } else {
            variant_count < (1usize << bits)
        };

        // Get the storage value ready for matching - needs to become cast_repr type
        let storage_val = if is_endian {
            // Endian types have .value() method returning native primitive
            quote! { (storage.value() as #cast_repr) }
        } else if is_primitive(&target_ty) {
            // Native primitives may be wider than cast_repr, need explicit cast
            quote! { (storage as #cast_repr) }
        } else {
            // ux2 types - convert via Into to cast_repr
            quote! { #cast_repr::from(storage) }
        };

        // Convert from enum to storage type - widen if necessary
        let to_storage = if is_endian {
            quote! { #target_ty::new(self as #cast_repr as _) }
        } else if !is_primitive(&target_ty) {
            quote! { #target_ty::new(self as #cast_repr as _) } // ux2 types
        } else {
            quote! { (self as #cast_repr) as #target_ty }
        };

        impls.extend(quote! {
            impl #impl_generics ZoruaNative<#target_ty> for #ident #type_generics #where_clause {
                const IS_FALLIBLE: bool = #is_fallible;

                fn try_from_storage(storage: #target_ty) -> Result<Self, #target_ty> {
                    match #storage_val as #cast_repr {
                        #match_arms
                        _ => Err(storage),
                    }
                }

                fn to_storage(self) -> #target_ty {
                    #to_storage
                }
            }
        });
    }

    Ok(impls.into())
}

fn impl_zoruanative_struct(
    ast: &DeriveInput,
    data: &DataStruct,
) -> Result<TokenStream, syn::Error> {
    // Must be a newtype (single field)
    if data.fields.len() != 1 {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "ZoruaNative can only be derived for structs with exactly one field (newtypes)",
        ));
    }

    let field0 = data.fields.iter().next().unwrap();

    // Must be a tuple struct
    if field0.ident.is_some() {
        return Err(syn::Error::new_spanned(
            field0,
            "ZoruaNative can only be derived for tuple structs (use `struct Name(T)` syntax)",
        ));
    }

    // Must be #[repr(transparent)]
    if !get_repr_state(&ast.attrs)?.repr_transparent {
        return Err(syn::Error::new_spanned(
            &ast.ident,
            "ZoruaNative requires #[repr(transparent)] for newtype structs",
        ));
    }

    let wrapped_ty = &field0.ty;
    let ident = &ast.ident;
    let generic_params = &ast.generics.params;
    let (_, type_generics, where_clause) = ast.generics.split_for_impl();

    // Generate impl that delegates to inner type
    // Uses a generic Storage parameter bound by inner type's ZoruaNative impl
    let output = quote! {
        impl<Storage, #generic_params> ZoruaNative<Storage> for #ident #type_generics
        where
            #wrapped_ty: ZoruaNative<Storage>,
            Storage: Clone,
            #where_clause
        {
            // Delegate fallibility to the wrapped type
            const IS_FALLIBLE: bool = <#wrapped_ty as ZoruaNative<Storage>>::IS_FALLIBLE;

            fn try_from_storage(storage: Storage) -> Result<Self, Storage> {
                <#wrapped_ty as ZoruaNative<Storage>>::try_from_storage(storage)
                    .map(Self)
            }

            fn to_storage(self) -> Storage {
                <#wrapped_ty as ZoruaNative<Storage>>::to_storage(self.0)
            }
        }
    };

    Ok(output.into())
}

// --------------
// Helper fns
// --------------
fn is_primitive(ty: &Type) -> bool {
    if let Type::Path(tp) = ty {
        if let Some(segment) = tp.path.segments.last() {
            let name = segment.ident.to_string();
            return matches!(name.as_str(), "u8" | "u16" | "u32" | "u64");
        }
    }
    false
}

fn deconstruct_array(ty: &Type) -> Option<&Type> {
    if let Type::Array(ta) = ty {
        Some(&*ta.elem)
    } else {
        None
    }
}

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
    is_fallible: bool, // #[fallible] attribute present
    bitfield_subfields: Option<Vec<BitfieldSubfield>>,
}

struct BitfieldSubfield {
    attrs: Vec<Attribute>,
    vis: Visibility,
    name: Ident,
    native_type: proc_macro2::TokenStream,
    storage_type: proc_macro2::TokenStream, // Same as native_type if no `as` clause
    has_backing_type: bool,
    is_fallible: bool,     // #[fallible] attribute present
    is_zeroedoption: bool, // #[zeroedoption] attribute present
    bit_offset: LitInt,
}

impl Parse for ZoruaField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let all_attrs = input.call(Attribute::parse_outer)?;

        // Check for and extract field-level attributes
        let is_fallible = all_attrs.iter().any(|a| a.path().is_ident("fallible"));

        // Filter out processed attributes - they shouldn't appear in generated code
        let attrs: Vec<_> = all_attrs
            .into_iter()
            .filter(|a| !a.path().is_ident("fallible") && !a.path().is_ident("zeroedoption"))
            .collect();

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
            is_fallible,
            bitfield_subfields,
        })
    }
}

impl Parse for BitfieldSubfield {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let all_attrs = input.call(Attribute::parse_outer)?;

        // Check for and extract field-level attributes
        let is_fallible = all_attrs.iter().any(|a| a.path().is_ident("fallible"));
        let is_zeroedoption = all_attrs.iter().any(|a| a.path().is_ident("zeroedoption"));

        // Filter out processed attributes - they shouldn't appear in generated code
        let attrs: Vec<_> = all_attrs
            .into_iter()
            .filter(|a| !a.path().is_ident("fallible") && !a.path().is_ident("zeroedoption"))
            .collect();

        let vis: Visibility = input.parse()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;

        // Parse the type - collect tokens until we hit @ or `as`
        let mut native_type_tokens = proc_macro2::TokenStream::new();
        let mut depth = 0; // Track <> nesting

        loop {
            if input.is_empty() {
                return Err(syn::Error::new(name.span(), "Expected @ after type"));
            }

            // Check for terminators (at depth 0)
            if depth == 0 {
                if input.peek(Token![as]) || input.peek(Token![@]) {
                    break;
                }
            }

            // Track <> nesting for generics like Result<Language, u8>
            if input.peek(Token![<]) {
                depth += 1;
            } else if input.peek(Token![>]) {
                depth -= 1;
            }

            // Consume next token
            let tt: proc_macro2::TokenTree = input.parse()?;
            native_type_tokens.extend(std::iter::once(tt));
        }

        // Check for `as StorageType`
        let (storage_type, has_backing_type) = if input.peek(Token![as]) {
            input.parse::<Token![as]>()?;

            // Parse storage type tokens until @
            let mut storage_tokens = proc_macro2::TokenStream::new();
            let mut depth = 0;

            loop {
                if input.is_empty() {
                    return Err(syn::Error::new(
                        name.span(),
                        "Expected @ after storage type",
                    ));
                }

                if depth == 0 && input.peek(Token![@]) {
                    break;
                }

                if input.peek(Token![<]) {
                    depth += 1;
                } else if input.peek(Token![>]) {
                    depth -= 1;
                }

                let tt: proc_macro2::TokenTree = input.parse()?;
                storage_tokens.extend(std::iter::once(tt));
            }

            (storage_tokens, true)
        } else {
            (native_type_tokens.clone(), false)
        };

        input.parse::<Token![@]>()?;
        let bit_offset: LitInt = input.parse()?;

        Ok(BitfieldSubfield {
            attrs,
            vis,
            name,
            native_type: native_type_tokens,
            storage_type,
            has_backing_type,
            is_fallible,
            is_zeroedoption,
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
            let field_ty = &f.storage_type;

            quote! {
                #(#field_attrs)*
                #field_vis #field_name: #field_ty
            }
        })
        .collect();

    // Generate field accessors (getter and setter) ONLY for backed fields
    let accessors: Vec<_> = fields
        .iter()
        .filter(|f| f.has_backing_type)
        .map(|f| {
            let field_attrs = &f.attrs;
            let field_vis = &f.vis;
            let field_name = &f.name;
            let field_raw_name = syn::Ident::new(&format!("{}_raw", f.name), f.name.span());
            let field_setter = syn::Ident::new(&format!("set_{}", f.name), f.name.span());
            let field_raw_setter = syn::Ident::new(&format!("set_{}_raw", f.name), f.name.span());
            let field_native_type = &f.native_type;
            let field_storage_type = &f.storage_type;

            let (getter, setter) = if let (Some(native_elem), Some(storage_elem)) = (deconstruct_array(field_native_type), deconstruct_array(field_storage_type)) {
                let is_identity = quote!(#native_elem).to_string() == quote!(#storage_elem).to_string();
                
                let assertion = if is_identity {
                    if f.is_fallible {
                         quote! {
                            compile_error!(concat!("Field `", stringify!(#field_name), "` marked #[fallible] but native type matches storage type (infallible)."));
                         }
                    } else {
                        quote! {}
                    }
                } else if f.is_fallible {
                    quote! {
                        const _: () = assert!(
                            <#native_elem as ZoruaNative<#storage_elem>>::IS_FALLIBLE,
                            concat!("Field `", stringify!(#field_name), "` marked #[fallible] but conversion is infallible.")
                        );
                    }
                } else {
                    quote! {
                        const _: () = assert!(
                            !<#native_elem as ZoruaNative<#storage_elem>>::IS_FALLIBLE,
                            concat!("Field `", stringify!(#field_name), "` missing #[fallible] but conversion is fallible.")
                        );
                    }
                };

                // Array Getter
                let getter = if is_identity {
                     quote! {
                        #field_vis fn #field_name(&self, index: usize) -> #native_elem {
                            #assertion
                            self.#field_raw_name[index]
                        }
                    }
                } else if f.is_fallible {
                    quote! {
                        #field_vis fn #field_name(&self, index: usize) -> Result<#native_elem, #storage_elem> {
                            #assertion
                            let storage_val = self.#field_raw_name[index];
                            <#native_elem as ZoruaNative<#storage_elem>>::try_from_storage(storage_val)
                        }
                    }
                } else {
                    quote! {
                        #field_vis fn #field_name(&self, index: usize) -> #native_elem {
                            #assertion
                            let storage_val = self.#field_raw_name[index];
                            <#native_elem as ZoruaNative<#storage_elem>>::from_storage(storage_val)
                        }
                    }
                };

                // Array Setter
                let setter = if is_identity {
                     quote! {
                        #field_vis fn #field_setter(&mut self, index: usize, val: #native_elem) {
                            self.#field_raw_name[index] = val;
                        }
                    }
                } else {
                    quote! {
                        #field_vis fn #field_setter(&mut self, index: usize, val: #native_elem) {
                            let storage_val = <#native_elem as ZoruaNative<#storage_elem>>::to_storage(val);
                            self.#field_raw_name[index] = storage_val;
                        }
                    }
                };
                (getter, setter)
            } else {
                let is_identity = quote!(#field_native_type).to_string() == quote!(#field_storage_type).to_string();

                let assertion = if is_identity {
                     if f.is_fallible {
                         quote! {
                            compile_error!(concat!("Field `", stringify!(#field_name), "` marked #[fallible] but native type matches storage type (infallible)."));
                         }
                    } else {
                        quote! {}
                    }
                } else if f.is_fallible {
                    quote! {
                        const _: () = assert!(
                            <#field_native_type as ZoruaNative<#field_storage_type>>::IS_FALLIBLE,
                            concat!("Field `", stringify!(#field_name), "` marked #[fallible] but conversion is infallible.")
                        );
                    }
                } else {
                    quote! {
                        const _: () = assert!(
                            !<#field_native_type as ZoruaNative<#field_storage_type>>::IS_FALLIBLE,
                            concat!("Field `", stringify!(#field_name), "` missing #[fallible] but conversion is fallible.")
                        );
                    }
                };

                // Non-Array Getter
                let getter = if is_identity {
                     quote! {
                        #field_vis fn #field_name(&self) -> #field_native_type {
                            #assertion
                            self.#field_raw_name()
                        }
                    }
                } else if f.is_fallible {
                    quote! {
                        #field_vis fn #field_name(&self) -> Result<#field_native_type, #field_storage_type> {
                            #assertion
                            let storage_val = self.#field_raw_name();
                            <#field_native_type as ZoruaNative<#field_storage_type>>::try_from_storage(storage_val)
                        }
                    }
                } else {
                    quote! {
                        #field_vis fn #field_name(&self) -> #field_native_type {
                            #assertion
                            let storage_val = self.#field_raw_name();
                            <#field_native_type as ZoruaNative<#field_storage_type>>::from_storage(storage_val)
                        }
                    }
                };

                // Non-Array Setter
                let setter = if is_identity {
                    quote! {
                        #field_vis fn #field_setter(&mut self, val: #field_native_type) {
                            self.#field_raw_setter(val);
                        }
                    }
                } else {
                     quote! {
                        #field_vis fn #field_setter(&mut self, val: #field_native_type) {
                            let storage_val = <#field_native_type as ZoruaNative<#field_storage_type>>::to_storage(val);
                            self.#field_raw_setter(storage_val);
                        }
                    }
                };
                (getter, setter)
            };

            quote! {
                #(#field_attrs)*
                #getter

                #(#field_attrs)*
                #setter
            }
        })
        .collect();

    // Generate raw accessors ONLY for non-array backed fields
    let raw_accessors: Vec<_> = fields
        .iter()
        .filter(|f| f.has_backing_type && deconstruct_array(&f.native_type).is_none())
        .map(|f| {
            let field_attrs = &f.attrs;
            let field_vis = &f.vis;
            let field_raw_name = syn::Ident::new(&format!("{}_raw", f.name), f.name.span());
            let field_raw_setter = syn::Ident::new(&format!("set_{}_raw", f.name), f.name.span());
            let field_storage_type = &f.storage_type;

            quote! {
                #(#field_attrs)*
                #field_vis fn #field_raw_name(&self) -> #field_storage_type {
                    self.#field_raw_name
                }
                
                #(#field_attrs)*
                #field_vis fn #field_raw_setter(&mut self, val: #field_storage_type) {
                    self.#field_raw_name = val;
                }
            }
        })
        .collect();

    // Generate bitfield subfield accessors
    let bitfield_accessors: Vec<_> = fields
        .iter()
        .filter_map(|f| {
            f.bitfield_subfields.as_ref().map(|subfields| {
                let container_raw_name = if f.has_backing_type {
                    syn::Ident::new(&format!("{}_raw", f.name), f.name.span())
                } else {
                    f.name.clone()
                };

                subfields.iter().map(move |sf| {
                    let sf_attrs = &sf.attrs;
                    let sf_vis = &sf.vis;
                    let sf_name = &sf.name;
                    let sf_setter = syn::Ident::new(&format!("set_{}", sf.name), sf.name.span());
                    let sf_native_type_ts = &sf.native_type;
                    let sf_storage_type_ts = &sf.storage_type;
                    let sf_offset = &sf.bit_offset;

                    // Parse the types from TokenStreams to check for arrays
                    let sf_native_type: Type = syn::parse2(sf_native_type_ts.clone()).unwrap();
                    let sf_storage_type: Type = syn::parse2(sf_storage_type_ts.clone()).unwrap();

                    if sf.has_backing_type {
                        // Aliased bitfield subfield: generate both aliased and _raw accessors
                        let sf_raw_name = syn::Ident::new(&format!("{}_raw", sf.name), sf.name.span());
                        let sf_raw_setter = syn::Ident::new(&format!("set_{}_raw", sf.name), sf.name.span());

                        if let (Some(native_elem), Some(storage_elem)) = (deconstruct_array(&sf_native_type), deconstruct_array(&sf_storage_type)) {
                            // Array-based bitfield subfield: generate indexed accessors
                            let is_identity = quote!(#native_elem).to_string() == quote!(#storage_elem).to_string();

                            let assertion = if is_identity {
                                if sf.is_fallible {
                                     quote! {
                                        compile_error!(concat!("Field `", stringify!(#sf_name), "` marked #[fallible] but native type matches storage type (infallible)."));
                                     }
                                } else {
                                    quote! {}
                                }
                            } else if sf.is_fallible {
                                quote! {
                                    const _: () = assert!(
                                        <#native_elem as ZoruaNative<#storage_elem>>::IS_FALLIBLE,
                                        concat!("Field `", stringify!(#sf_name), "` marked #[fallible] but conversion is infallible.")
                                    );
                                }
                            } else {
                                quote! {
                                    const _: () = assert!(
                                        !<#native_elem as ZoruaNative<#storage_elem>>::IS_FALLIBLE,
                                        concat!("Field `", stringify!(#sf_name), "` missing #[fallible] but conversion is fallible.")
                                    );
                                }
                            };

                            let getter = if is_identity {
                                quote! {
                                    #sf_vis fn #sf_name(&self, index: usize) -> #native_elem {
                                        #assertion
                                        let b_bits = <#native_elem as BackingStorage>::BITS;
                                        self.#container_raw_name.get_bits_at::<#native_elem>(#sf_offset + index * b_bits)
                                    }
                                }
                            } else if sf.is_fallible {
                                quote! {
                                    #sf_vis fn #sf_name(&self, index: usize) -> Result<#native_elem, #storage_elem> {
                                        #assertion
                                        let b_bits = <#storage_elem as BackingStorage>::BITS;
                                        let storage_val = self.#container_raw_name.get_bits_at::<#storage_elem>(#sf_offset + index * b_bits);
                                        <#native_elem as ZoruaNative<#storage_elem>>::try_from_storage(storage_val)
                                    }
                                }
                            } else {
                                quote! {
                                    #sf_vis fn #sf_name(&self, index: usize) -> #native_elem {
                                        #assertion
                                        let b_bits = <#storage_elem as BackingStorage>::BITS;
                                        let storage_val = self.#container_raw_name.get_bits_at::<#storage_elem>(#sf_offset + index * b_bits);
                                        <#native_elem as ZoruaNative<#storage_elem>>::from_storage(storage_val)
                                    }
                                }
                            };

                            let setter = if is_identity {
                                 quote! {
                                    #sf_vis fn #sf_setter(&mut self, index: usize, val: #native_elem) {
                                        let b_bits = <#native_elem as BackingStorage>::BITS;
                                        self.#container_raw_name.set_bits_at::<#native_elem>(val, #sf_offset + index * b_bits);
                                    }
                                }
                            } else {
                                quote! {
                                    #sf_vis fn #sf_setter(&mut self, index: usize, val: #native_elem) {
                                        let b_bits = <#storage_elem as BackingStorage>::BITS;
                                        let storage_val = <#native_elem as ZoruaNative<#storage_elem>>::to_storage(val);
                                        self.#container_raw_name.set_bits_at::<#storage_elem>(storage_val, #sf_offset + index * b_bits);
                                    }
                                }
                            };

                            quote! {
                                #(#sf_attrs)*
                                #getter
                                #(#sf_attrs)*
                                #setter

                                // Raw accessors for the whole array
                                #(#sf_attrs)*
                                #sf_vis fn #sf_raw_name(&self) -> #sf_storage_type {
                                    self.#container_raw_name.get_bits_at::<#sf_storage_type>(#sf_offset)
                                }
                                #(#sf_attrs)*
                                #sf_vis fn #sf_raw_setter(&mut self, val: #sf_storage_type) {
                                    self.#container_raw_name.set_bits_at::<#sf_storage_type>(val, #sf_offset);
                                }
                            }
                        } else if sf.is_zeroedoption {
                            // Non-fallible zeroedoption accessor: returns Option<NativeType>
                            let is_identity = quote!(#sf_native_type).to_string() == quote!(#sf_storage_type).to_string();
                            
                            if is_identity {
                                quote! {
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_name(&self) -> Option<#sf_native_type> {
                                        let storage_val = self.#sf_raw_name();
                                        if storage_val == <#sf_storage_type as BackingStorage>::ZERO {
                                            None
                                        } else {
                                            Some(storage_val)
                                        }
                                    }

                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_setter(&mut self, val: Option<#sf_native_type>) {
                                        let storage_val = match val {
                                            None => <#sf_storage_type as BackingStorage>::ZERO,
                                            Some(v) => v,
                                        };
                                        self.#sf_raw_setter(storage_val);
                                    }
                                    
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_name(&self) -> #sf_storage_type {
                                        self.#container_raw_name.get_bits_at::<#sf_storage_type>(#sf_offset)
                                    }

                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_setter(&mut self, val: #sf_storage_type) {
                                        self.#container_raw_name.set_bits_at::<#sf_storage_type>(val, #sf_offset);
                                    }
                                }
                            } else {
                                quote! {
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_name(&self) -> Option<#sf_native_type> {
                                        let storage_val = self.#sf_raw_name();
                                        if storage_val == <#sf_storage_type as BackingStorage>::ZERO {
                                            None
                                        } else {
                                            Some(<#sf_native_type as ZoruaNative<#sf_storage_type>>::from_storage(storage_val))
                                        }
                                    }

                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_setter(&mut self, val: Option<#sf_native_type>) {
                                        let storage_val = match val {
                                            None => <#sf_storage_type as BackingStorage>::ZERO,
                                            Some(v) => <#sf_native_type as ZoruaNative<#sf_storage_type>>::to_storage(v),
                                        };
                                        self.#sf_raw_setter(storage_val);
                                    }

                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_name(&self) -> #sf_storage_type {
                                        self.#container_raw_name.get_bits_at::<#sf_storage_type>(#sf_offset)
                                    }

                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_setter(&mut self, val: #sf_storage_type) {
                                        self.#container_raw_name.set_bits_at::<#sf_storage_type>(val, #sf_offset);
                                    }
                                }
                            }
                        } else if sf.is_fallible {
                            let is_identity = quote!(#sf_native_type).to_string() == quote!(#sf_storage_type).to_string();
                            if is_identity {
                                 quote! {
                                    compile_error!(concat!("Field `", stringify!(#sf_name), "` marked #[fallible] but native type matches storage type (infallible)."));
                                 }
                            } else {
                                quote! {
                                    // Fallible aliased getter - returns Result<NativeType, StorageType>
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_name(&self) -> Result<#sf_native_type, #sf_storage_type> {
                                        const _: () = assert!(
                                            <#sf_native_type as ZoruaNative<#sf_storage_type>>::IS_FALLIBLE,
                                            concat!("Field `", stringify!(#sf_name), "` marked #[fallible] but conversion is infallible.")
                                        );
                                        let storage_val = self.#sf_raw_name();
                                        <#sf_native_type as ZoruaNative<#sf_storage_type>>::try_from_storage(storage_val)
                                    }

                                    // Aliased setter
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_setter(&mut self, val: #sf_native_type) {
                                        let storage_val = <#sf_native_type as ZoruaNative<#sf_storage_type>>::to_storage(val);
                                        self.#sf_raw_setter(storage_val);
                                    }
                                    
                                    // Raw getter - returns StorageType directly
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_name(&self) -> #sf_storage_type {
                                        self.#container_raw_name.get_bits_at::<#sf_storage_type>(#sf_offset)
                                    }

                                    // Raw setter
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_setter(&mut self, val: #sf_storage_type) {
                                        self.#container_raw_name.set_bits_at::<#sf_storage_type>(val, #sf_offset);
                                    }
                                }
                            }
                        } else {
                            let is_identity = quote!(#sf_native_type).to_string() == quote!(#sf_storage_type).to_string();
                            if is_identity {
                                quote! {
                                    // Identity accessors
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_name(&self) -> #sf_native_type {
                                        const _: () = assert!(
                                            !<#sf_native_type as ZoruaNative<#sf_storage_type>>::IS_FALLIBLE,
                                            concat!("Field `", stringify!(#sf_name), "` missing #[fallible] but conversion is fallible.")
                                        );
                                        self.#sf_raw_name()
                                    }
    
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_setter(&mut self, val: #sf_native_type) {
                                        self.#sf_raw_setter(val);
                                    }
                                    
                                    // Raw getter - returns StorageType directly
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_name(&self) -> #sf_storage_type {
                                        self.#container_raw_name.get_bits_at::<#sf_storage_type>(#sf_offset)
                                    }
    
                                    // Raw setter
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_setter(&mut self, val: #sf_storage_type) {
                                        self.#container_raw_name.set_bits_at::<#sf_storage_type>(val, #sf_offset);
                                    }
                                }
                            } else {
                                quote! {
                                    // Aliased getter - returns NativeType via ZoruaNative<StorageType>
                                    #(#sf_attrs)*
                                        #sf_vis fn #sf_name(&self) -> #sf_native_type {
                                            const _: () = assert!(
                                                !<#sf_native_type as ZoruaNative<#sf_storage_type>>::IS_FALLIBLE,
                                                concat!("Field `", stringify!(#sf_name), "` missing #[fallible] but conversion is fallible.")
                                            );
                                            let storage_val = self.#sf_raw_name();
                                            <#sf_native_type as ZoruaNative<#sf_storage_type>>::from_storage(storage_val)
                                        }
    
                                    // Aliased setter
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_setter(&mut self, val: #sf_native_type) {
                                        let storage_val = <#sf_native_type as ZoruaNative<#sf_storage_type>>::to_storage(val);
                                        self.#sf_raw_setter(storage_val);
                                    }
                                    
                                    // Raw getter - returns StorageType directly
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_name(&self) -> #sf_storage_type {
                                        self.#container_raw_name.get_bits_at::<#sf_storage_type>(#sf_offset)
                                    }
    
                                    // Raw setter
                                    #(#sf_attrs)*
                                    #sf_vis fn #sf_raw_setter(&mut self, val: #sf_storage_type) {
                                        self.#container_raw_name.set_bits_at::<#sf_storage_type>(val, #sf_offset);
                                    }
                                }
                            }
                        }
                    } else {
                        // Direct bitfield subfield (no `as`): generate direct accessors
                        if let (Some(native_elem), Some(_)) = (deconstruct_array(&sf_native_type), deconstruct_array(&sf_storage_type)) {
                            let sf_raw_name = syn::Ident::new(&format!("{}_raw", sf.name), sf.name.span());
                            let sf_raw_setter = syn::Ident::new(&format!("set_{}_raw", sf.name), sf.name.span());
                            
                            quote! {
                                #(#sf_attrs)*
                                #sf_vis fn #sf_name(&self, index: usize) -> #native_elem {
                                    let b_bits = <#native_elem as BackingStorage>::BITS;
                                    self.#container_raw_name.get_bits_at::<#native_elem>(#sf_offset + index * b_bits)
                                }

                                #(#sf_attrs)*
                                #sf_vis fn #sf_setter(&mut self, index: usize, val: #native_elem) {
                                    let b_bits = <#native_elem as BackingStorage>::BITS;
                                    self.#container_raw_name.set_bits_at::<#native_elem>(val, #sf_offset + index * b_bits);
                                }

                                // Raw accessors for the whole array
                                #(#sf_attrs)*
                                #sf_vis fn #sf_raw_name(&self) -> #sf_native_type {
                                    self.#container_raw_name.get_bits_at::<#sf_native_type>(#sf_offset)
                                }
                                
                                #(#sf_attrs)*
                                #sf_vis fn #sf_raw_setter(&mut self, val: #sf_native_type) {
                                    self.#container_raw_name.set_bits_at::<#sf_native_type>(val, #sf_offset);
                                }
                            }
                        } else {
                            quote! {
                                #(#sf_attrs)*
                                #sf_vis fn #sf_name(&self) -> #sf_native_type {
                                    self.#container_raw_name.get_bits_at::<#sf_native_type>(#sf_offset)
                                }

                                #(#sf_attrs)*
                                #sf_vis fn #sf_setter(&mut self, val: #sf_native_type) {
                                    self.#container_raw_name.set_bits_at::<#sf_native_type>(val, #sf_offset);
                                }
                            }
                        }
                    }
                })
            })
        })
        .flatten()
        .collect();

    let output = quote! {
        #(#attrs)*
        #vis struct #name #generics #where_clause {
            #(#field_defs),*
        }

        impl #impl_generics #name #ty_generics #where_clause {
            #(#accessors)*
            #(#raw_accessors)*
            #(#bitfield_accessors)*
        }
    };

    Ok(output.into())
}
