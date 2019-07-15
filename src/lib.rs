#![allow(dead_code)]
#![cfg_attr(not(feature = "std"), no_std)]
#![warn(missing_docs)]

#[macro_use]
#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
extern crate core as std;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
mod token;
pub mod validator;
