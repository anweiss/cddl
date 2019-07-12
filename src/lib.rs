#![allow(dead_code)]
#![cfg_attr(not(feature = "std"), no_std)]

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
mod token;
pub mod validator;
