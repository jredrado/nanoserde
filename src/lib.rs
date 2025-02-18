#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(not(feature = "std"), no_main)]

//! # nanoserde

//! [![Github Actions](https://github.com/not-fl3/nanoserde/workflows/Cross-compile/badge.svg)](https://github.com/not-fl3/nanoserde/actions?query=workflow%3A)
//! [![Crates.io version](https://img.shields.io/crates/v/nanoserde.svg)](https://crates.io/crates/nanoserde)
//! [![Documentation](https://docs.rs/nanoserde/badge.svg)](https://docs.rs/nanoserde)
//! [![Discord chat](https://img.shields.io/discord/710177966440579103.svg?label=discord%20chat)](https://discord.gg/WfEp6ut)
//!
//! Data serialization library with zero dependencies. No more syn/proc_macro2/quote in the build tree!
//!
//! `nanoserde` also almost do not use any generics, so build size is not going to be bloated with monomorphizated code.
//!
//! The main difference with "serde" and the reason why "nanoserde" is possible: there is no intermediate data model
//! For each serialisation datatype there is a special macro.
//!
//! Derive macros available: `DeJson`, `SerJson`, `DeBin`, `SerBin`, `DeRon`, `SerRon`
//!
//! `nanoserde` supports some serialization customisation with `#[nserde()]` attributes.
//! For `#[nserde(..)]` supported attributes for each format check [Features support matrix](https://github.com/not-fl3/nanoserde#features-support-matrix)

pub use nanoserde_derive::*;

extern crate alloc;


mod serde_bin;
pub use crate::serde_bin::*;

mod serde_ron;
pub use crate::serde_ron::*;

mod serde_json;
pub use crate::serde_json::*;


mod toml;
pub use crate::toml::*;

