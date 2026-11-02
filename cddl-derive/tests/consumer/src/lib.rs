// An independent workspace: cddl-derive's dev-dependencies cannot satisfy these paths.
cddl_derive::cddl_typegen!("schema.cddl");

pub mod fundamental {
  cddl_derive::cddl_typegen!("schema.cddl", fundamental_aliases = true);
}

pub mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

pub mod generated_fundamental {
  include!(concat!(env!("OUT_DIR"), "/fundamental.rs"));
}

#[cfg(test)]
mod tests {
  #[test]
  fn generated_types_work_in_a_downstream_crate() {
    let record = super::Record {
      hash: vec![1, 2, 3],
      when: "2026-09-24T00:00:00Z".into(),
      payload: serde_json::json!({"value": 1}),
    };
    let mut bytes = Vec::new();
    ciborium::into_writer(&record, &mut bytes).unwrap();
    let decoded: super::Record = ciborium::from_reader(bytes.as_slice()).unwrap();
    assert_eq!(decoded.hash, record.hash);
    assert_eq!(decoded.when, record.when);
    assert_eq!(decoded.payload, record.payload);
    let json = serde_json::to_string(&record).unwrap();
    let decoded: super::Record = serde_json::from_str(&json).unwrap();
    assert_eq!(decoded.hash, record.hash);
    let aliased: super::fundamental::Record = serde_json::from_str(&json).unwrap();
    let mut aliased_bytes = Vec::new();
    ciborium::into_writer(&aliased, &mut aliased_bytes).unwrap();
    assert_eq!(aliased_bytes, bytes);
    let decoded: super::fundamental::Record = ciborium::from_reader(bytes.as_slice()).unwrap();
    assert_eq!(decoded.when, record.when);

    let generated: super::generated::Record = serde_json::from_str(&json).unwrap();
    let mut generated_bytes = Vec::new();
    ciborium::into_writer(&generated, &mut generated_bytes).unwrap();
    assert_eq!(generated_bytes, bytes);
    let decoded: super::generated::Record =
      ciborium::from_reader(generated_bytes.as_slice()).unwrap();
    assert_eq!(decoded.hash, record.hash);

    let generated: super::generated_fundamental::Record = serde_json::from_str(&json).unwrap();
    let mut generated_bytes = Vec::new();
    ciborium::into_writer(&generated, &mut generated_bytes).unwrap();
    assert_eq!(generated_bytes, bytes);
  }
}
