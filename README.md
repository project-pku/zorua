# Zorua

Zorua is a Rust library for safe, zero-copy transmutation of binary data, with a focus on bit-packed structures and endian-aware fields.

## ZoruaField

The `ZoruaField` trait ensures that a type can be safely transmuted from a raw byte buffer. It enforces several safety conditions at compile time:
- The type must have a stable layout (`#[repr(C)]` for structs, `#[repr(u8)]` for enums).
- The type must have an **alignment of 1** (this ensures no padding exists in `#[repr(C)]` structures).
- All fields must also implement `ZoruaField`.

```rust
#[repr(C)]
#[derive(ZoruaField)]
pub struct SimpleStruct {
    pub a: u8,
    pub b: [u8; 3],
}
```

## bitfields!

The `bitfields!` macro is the core of Zorua. It allows you to define complex, bit-packed structures with ease.

### Features
- **Endian-aware primitives**: Support for `u16_le`, `u32_be`, etc.
- **Native type mapping**: Map storage types to high-level Rust types using the `as` syntax.
- **Bit-packed fields**: Define fields that occupy a specific range of bits within a larger integer.
- **Array bitfields**: Map multiple bits to an array of smaller bit-types (e.g. `[u4; 2]`).
- **Fallibility**: Automatic handling of fallible conversions with `#[fallible]` or `#[zeroedoption]`.

```rust
bitfields! {
    #[repr(C)]
    #[derive(ZoruaField, Clone, Debug)]
    pub struct PokemonSubstructure {
        pub species: SpeciesId as u16_le,
        pub item: ItemId as u16_le,
        pub experience: u32 as u32_le,
        
        // Bit-packed fields within a u16
        misc_flags: u16_le {
            pub met_level: u7@0,
            pub origin_game: GameId as u4@7,
            pub pokeball: BallId as u4@11,
            pub ot_gender: Gender as u1@15,
        },
    }
}
```

## ZoruaNative

The `ZoruaNative` trait defines how a high-level "native" type is converted to and from its binary "storage" representation.

- **Infallible**: For types where any binary value is valid (e.g. `u32` -> `u32_le`).
- **Fallible**: For types where only some values are valid (e.g. enums).

```rust
#[repr(u8)]
#[derive(ZoruaNative, Clone, Copy, Debug)]
pub enum Language {
    Japanese = 1,
    English = 2,
    French = 3,
    // ...
}
```


## No-Std Support

`zorua` is `no_std` by default. To enable `std` features (like `Box` transmutation), use:

```toml
[dependencies]
zorua = { ... features = ["std"] }
```
