//! # cddl
//!
//! CDDL is an IETF standard that "proposes a notational convention to express
//! CBOR and JSON data structures." As of 2019-06-12, it is published as RFC
//! 8610 (Proposed Standard) at https://tools.ietf.org/html/rfc8610.
//!
//! ```cddl
//! reputation-object = {
//!   reputation-context,
//!   reputon-list
//! }
//!
//! reputation-context = (
//!   application: text
//! )
//!
//! reputon-list = (
//!   reputons: reputon-array
//! )
//!
//! reputon-array = [* reputon]
//!
//! reputon = {
//!   rater-value,
//!   assertion-value,
//!   rated-value,
//!   rating-value,
//!   ? conf-value,
//!   ? normal-value,
//!   ? sample-value,
//!   ? gen-value,
//!   ? expire-value,
//!   * ext-value,
//! }
//!
//! rater-value = ( rater: text )
//! assertion-value = ( assertion: text )
//! rated-value = ( rated: text )
//! rating-value = ( rating: float16 )
//! conf-value = ( confidence: float16 )
//! normal-value = ( normal-rating: float16 )
//! sample-value = ( sample-size: uint )
//! gen-value = ( generated: uint )
//! expire-value = ( expires: uint )
//! ext-value = ( text => any )
//! ```

#![allow(dead_code)]
#![cfg_attr(not(feature = "std"), no_std)]
#![warn(missing_docs)]

#[macro_use]
#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
extern crate core as std;

#[macro_use]
extern crate serde_json;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
mod token;
pub mod validator;

#[doc(inline)]
pub use self::parser::{cddl_from_str, compile};
