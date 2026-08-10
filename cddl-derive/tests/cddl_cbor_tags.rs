use cddl_derive::cddl_typegen;

// Regression test for https://github.com/anweiss/cddl/issues/639
//
// `tdate` is `#6.0(tstr)` and `uri` is `#6.32(tstr)`, so they must serialize as
// tagged CBOR values. The generated Rust type stays `String`; the tag is
// applied by a serde helper, and only for non-human-readable formats so that
// JSON output is unaffected.
cddl_typegen!("tests/fixtures/cbor_tags.cddl");

fn to_cbor<T: serde::Serialize>(value: &T) -> Vec<u8> {
  let mut buf = Vec::new();
  ciborium::into_writer(value, &mut buf).unwrap();
  buf
}

#[test]
fn tdate_serializes_as_cbor_tag_0() {
  let r = TdateRecord {
    value: "2020-01-01T00:00:00Z".to_string(),
  };
  let encoded = to_cbor(&r);

  // 0xc0 is tag 0.
  assert!(
    encoded.contains(&0xc0),
    "expected tag 0 in {:02x?}",
    encoded
  );

  let decoded: TdateRecord = ciborium::from_reader(encoded.as_slice()).unwrap();
  assert_eq!(decoded.value, "2020-01-01T00:00:00Z");
}

#[test]
fn uri_serializes_as_cbor_tag_32() {
  let r = UriRecord {
    value: "https://example.com".to_string(),
  };
  let encoded = to_cbor(&r);

  // 0xd8 0x20 is tag 32.
  assert!(
    encoded.windows(2).any(|w| w == [0xd8, 0x20]),
    "expected tag 32 in {:02x?}",
    encoded
  );

  let decoded: UriRecord = ciborium::from_reader(encoded.as_slice()).unwrap();
  assert_eq!(decoded.value, "https://example.com");
}

#[test]
fn tags_do_not_leak_into_json() {
  let r = TdateRecord {
    value: "2020-01-01T00:00:00Z".to_string(),
  };

  let json = serde_json::to_string(&r).unwrap();
  assert_eq!(json, r#"{"value":"2020-01-01T00:00:00Z"}"#);

  let decoded: TdateRecord = serde_json::from_str(&json).unwrap();
  assert_eq!(decoded.value, "2020-01-01T00:00:00Z");
}

#[test]
fn mixed_and_optional_tagged_fields_round_trip() {
  let r = TaggedRecord {
    created: "2020-01-01T00:00:00Z".to_string(),
    link: "https://example.com".to_string(),
    epoch: 1577836800,
    optional_link: Some("https://example.org".to_string()),
  };

  let decoded: TaggedRecord = ciborium::from_reader(to_cbor(&r).as_slice()).unwrap();
  assert_eq!(decoded.created, r.created);
  assert_eq!(decoded.link, r.link);
  assert_eq!(decoded.epoch, r.epoch);
  assert_eq!(decoded.optional_link, r.optional_link);

  let absent = TaggedRecord {
    optional_link: None,
    ..r
  };
  let decoded: TaggedRecord = ciborium::from_reader(to_cbor(&absent).as_slice()).unwrap();
  assert_eq!(decoded.optional_link, None);
}

#[test]
fn untagged_input_is_still_accepted() {
  // A producer that omits the tag should still decode.
  let mut buf = Vec::new();
  let mut map = std::collections::HashMap::new();
  map.insert("value".to_string(), "2020-01-01T00:00:00Z".to_string());
  ciborium::into_writer(&map, &mut buf).unwrap();

  let decoded: TdateRecord = ciborium::from_reader(buf.as_slice()).unwrap();
  assert_eq!(decoded.value, "2020-01-01T00:00:00Z");
}
