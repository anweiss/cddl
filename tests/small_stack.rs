#![cfg(all(feature = "native-stack", not(target_arch = "wasm32")))]

use std::thread;

// Pest 2.9 rejects any parse with less than 64 KiB of native stack remaining.
const SMALL_STACK: usize = 64 * 1024;

fn on_small_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
  thread::Builder::new()
    .stack_size(SMALL_STACK)
    .spawn(f)
    .unwrap()
    .join()
    .unwrap()
}

#[test]
fn parses_cddl_on_small_thread_stack() {
  let result = on_small_stack(|| {
    cddl::cddl_from_str("person = { name: tstr, age: uint }\n", false).map(|cddl| cddl.rules.len())
  });
  assert_eq!(result, Ok(1));
}

#[test]
fn reports_cddl_syntax_errors_on_small_thread_stack() {
  let input = "person = {\n";
  let expected = cddl::cddl_from_str(input, false).unwrap_err();
  let actual = on_small_stack(move || cddl::cddl_from_str(input, false).unwrap_err());
  assert_eq!(actual, expected);
}
