rkyv_rpc
========

[![crates.io](https://img.shields.io/crates/v/rkyv_rpc.svg)](https://crates.io/crates/rkyv_rpc)
[![Documentation](https://docs.rs/rkyv_rpc/badge.svg)](https://docs.rs/rkyv_rpc)
[![MIT/Apache-2 licensed](https://img.shields.io/crates/l/rkyv_rpc.svg)](./LICENSE-Apache)

# Synopsis

This crate provides useful macros and utilities for working with rkyv objects in a networked environment, such as RPC commands. It helps
to ensure that RPC commands and other data are cross-endian and **backwards compatible**.

Unlike `#[derive(rkyv::Archive)]`, enums created by this implementation use a custom enum discriminator for which the bytes are mirrored,
resulting in the same representation regardless of endianness. Furthermore, this allows for discriminators larger than 1 byte,
allowing one to space out custom discriminator values to allow for older software
to still accept the same data structures while gracefully failing to recognize newer discriminants.

A framed codec is also provided for safely reading and writing binary streams of rkyv objects.

# Cargo Features
* `codec` (default) - Enables the `Encoder`/`Decoder` implementations to write/read rkyv objects to byte buffers and `AsyncWrite`/`AsyncRead` streams via tokio's `Framed`
* `unaligned` - Enable unaligned accesses by default for rkyv. This saves an extra clone of the incoming bytes before decoding.
* `bitflags` - Enable a macro to wrap generation of bitflags to be used safely with `rkyv`.