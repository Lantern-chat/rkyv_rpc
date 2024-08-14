rkyv_rpc
========

[![crates.io](https://img.shields.io/crates/v/rkyv_rpc.svg)](https://crates.io/crates/rkyv_rpc)
[![Documentation](https://docs.rs/rkyv_rpc/badge.svg)](https://docs.rs/rkyv_rpc)
[![MIT/Apache-2 licensed](https://img.shields.io/crates/l/rkyv_rpc.svg)](./LICENSE-Apache)

# Synopsis

This crate provides a macro to declare an enum specifically to carry RPC commands in such a way as to ensure cross-endian and backwards compatibility.

Unlike `#[derive(rkyv::Archive)]`, this implementation create a custom enum discriminator for which the bytes are mirrored, creating the same representation regardless
or endianness. Furthermore, this allows for discriminators larger than 1 byte, allowing one to space out custom discriminator values to allow for older software
to still accept the same data structures while gracefully failing to recognize newer discriminants.

Furthermore, a framed codec is provided for safely reading and writing binary streams of rkyv objects.

# Cargo Features
* `codec` (default) - Enables the `Encoder`/`Decoder` implementations to write/read rkyv objects to byte buffers and `AsyncWrite`/`AsyncRead` streams via tokio's `Framed`
* `unaligned` - Enable unaligned accesses by default for rkyv. This saves an extra clone of the incoming bytes before decoding.