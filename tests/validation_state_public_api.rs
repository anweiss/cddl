#![cfg(all(feature = "std", not(feature = "lsp")))]

#[test]
fn validation_state_retains_external_struct_update_construction() {
  let cddl = cddl::pest_bridge::cddl_from_pest_str("root = any").unwrap();
  let _state = cddl::validator::ValidationState {
    is_root: true,
    ..cddl::validator::ValidationState::new(
      &cddl,
      #[cfg(feature = "additional-controls")]
      None,
    )
  };
}
