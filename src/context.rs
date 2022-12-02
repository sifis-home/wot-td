use std::{collections::HashMap, fmt, ops::Deref};

use serde::{
    de::{self, Visitor},
    ser::SerializeSeq,
    Deserialize, Deserializer, Serialize, Serializer,
};
use serde_json::{Map, Value};

/// The JSON-LD context for the version 1.0 of the [Thing
/// description](https://www.w3.org/TR/wot-thing-description/)
pub const TD_CONTEXT_10: &str = "https://www.w3.org/2019/wot/td/v1";

/// The JSON-LD context for the version 1.1 of the [Thing
/// description](https://www.w3.org/TR/wot-thing-description11/)
pub const TD_CONTEXT_11: &str = "https://www.w3.org/2019/wot/td/v1.1";

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Context {
    pub default: Option<ContextEntry>,
    pub named: HashMap<String, ContextEntry>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum ContextEntry {
    Iri(Iri),
    Object(ContextEntryObject),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Iri(String);

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ContextEntryObject {
    #[serde(rename = "@id", skip_serializing_if = "Option::is_none")]
    iri: Option<Iri>,

    #[serde(flatten)]
    other: Map<String, Value>,
}

impl Iri {
    #[inline]
    pub fn new(iri: impl Into<String>) -> Result<Self, InvalidEmptyIri> {
        Self::try_from(iri.into())
    }

    #[inline]
    pub fn td_context_11() -> Self {
        Self(TD_CONTEXT_11.to_owned())
    }

    #[inline]
    pub fn td_context_10() -> Self {
        Self(TD_CONTEXT_10.to_owned())
    }

    #[inline]
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl Deref for Iri {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl AsRef<String> for Iri {
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl AsRef<str> for Iri {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Iri {
    type Error = InvalidEmptyIri;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(InvalidEmptyIri)
        } else {
            Ok(Self(value))
        }
    }
}

impl TryFrom<&str> for Iri {
    type Error = InvalidEmptyIri;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(InvalidEmptyIri)
        } else {
            Ok(Self(value.to_owned()))
        }
    }
}

impl<'de> Deserialize<'de> for Iri {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        String::deserialize(deserializer)
            .and_then(|iri| Iri::try_from(iri).map_err(de::Error::custom))
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidEmptyIri;

impl fmt::Display for InvalidEmptyIri {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("IRI cannot be an empty string")
    }
}

impl std::error::Error for InvalidEmptyIri {}

impl Context {
    #[inline]
    pub fn new(iri: impl Into<String>) -> Result<Self, InvalidEmptyIri> {
        ContextEntry::new(iri).map(|default| Self {
            default: Some(default),
            named: HashMap::new(),
        })
    }

    #[inline]
    #[cfg(test)]
    fn empty() -> Self {
        Self {
            default: None,
            named: HashMap::new(),
        }
    }
}

impl Default for Context {
    #[inline]
    fn default() -> Self {
        Self {
            default: Some(ContextEntry::Iri(Iri::td_context_11())),
            named: HashMap::new(),
        }
    }
}

impl<'de> Deserialize<'de> for Context {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ContextVisitor;
        impl<'de> Visitor<'de> for ContextVisitor {
            type Value = Context;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str(
                    "an IRI string, an array with an IRI string and one or more maps of context \
                     entries or a map of context entries",
                )
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Context::new(v).map_err(de::Error::custom)
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Context::new(v).map_err(de::Error::custom)
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut default = None;
                let mut named = HashMap::new();
                while let Some((name, value)) = map.next_entry()? {
                    visit_context_object_key_value(name, value, &mut default, &mut named)?;
                }

                Ok(Context { default, named })
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let mut default = None::<ContextEntry>;
                let mut named = HashMap::new();

                while let Some(element) = seq.next_element::<Value>()? {
                    match element {
                        Value::String(entry)
                            if default.as_ref().and_then(|entry| entry.iri()).is_none() =>
                        {
                            let iri = entry.try_into().map_err(de::Error::custom)?;
                            match &mut default {
                                Some(ContextEntry::Object(ContextEntryObject {
                                    iri: default_iri @ None,
                                    ..
                                })) => *default_iri = Some(iri),

                                None => default = Some(ContextEntry::Iri(iri)),

                                Some(
                                    ContextEntry::Iri(_)
                                    | ContextEntry::Object(ContextEntryObject {
                                        iri: Some(_), ..
                                    }),
                                ) => {
                                    unreachable!()
                                }
                            }
                        }
                        Value::String(_) => {
                            return Err(de::Error::custom(
                                "default context specified more than once",
                            ))
                        }
                        Value::Object(map) => {
                            for (name, value) in map {
                                visit_context_object_key_value(
                                    name,
                                    value,
                                    &mut default,
                                    &mut named,
                                )?;
                            }
                        }
                        _ => return Err(de::Error::custom(
                            "only (mixed) array of strings and objects are supported for contexts",
                        )),
                    }
                }

                Ok(Context { default, named })
            }
        }

        deserializer.deserialize_any(ContextVisitor)
    }
}
fn visit_context_object_key_value<E: de::Error>(
    key: String,
    value: Value,
    default: &mut Option<ContextEntry>,
    named: &mut HashMap<String, ContextEntry>,
) -> Result<(), E> {
    match (&*key, value) {
        ("@id", Value::String(iri)) if default.as_ref().and_then(|entry| entry.iri()).is_none() => {
            let iri = Iri::try_from(iri).map_err(de::Error::custom)?;
            match default {
                Some(ContextEntry::Object(ContextEntryObject {
                    iri: default_iri @ None,
                    ..
                })) => {
                    *default_iri = Some(iri);
                }

                None => {
                    *default = Some(ContextEntry::Object(ContextEntryObject {
                        iri: Some(iri),
                        other: Default::default(),
                    }))
                }

                Some(
                    ContextEntry::Iri(_)
                    | ContextEntry::Object(ContextEntryObject { iri: Some(_), .. }),
                ) => {
                    unreachable!()
                }
            }
            Ok(())
        }
        ("@id", Value::String(_)) => Err(de::Error::custom(
            "default context specified more than once",
        )),
        ("@id", _) => Err(de::Error::custom(
            "only string @id values in contexts are supported",
        )),
        (_, value) if key.starts_with('@') => match default {
            Some(ContextEntry::Iri(_)) => {
                let ContextEntry::Iri(iri) = default.take().unwrap() else { unreachable!() };
                *default = Some(ContextEntry::Object(ContextEntryObject {
                    iri: Some(iri),
                    other: [(key, value)].into_iter().collect(),
                }));
                Ok(())
            }
            Some(ContextEntry::Object(ContextEntryObject { other, .. })) => {
                other.insert(key, value);
                Ok(())
            }
            None => {
                *default = Some(ContextEntry::Object(ContextEntryObject {
                    iri: None,
                    other: [(key, value)].into_iter().collect(),
                }));
                Ok(())
            }
        },
        (_, Value::String(iri)) => {
            check_context_prefix(&key)?;
            let iri = Iri::try_from(iri).map_err(de::Error::custom)?;
            named.insert(key, ContextEntry::Iri(iri));
            Ok(())
        }
        (_, Value::Object(mut map)) => {
            check_context_prefix(&key)?;
            let iri = map
                .remove("@id")
                .map(|id| match id {
                    Value::String(iri) => Iri::try_from(iri).map_err(de::Error::custom),
                    _ => Err(de::Error::custom(
                        "only string @id values in contexts are supported",
                    )),
                })
                .transpose()?;

            named.insert(
                key,
                ContextEntry::Object(ContextEntryObject { iri, other: map }),
            );
            Ok(())
        }
        _ => Err(de::Error::custom(
            "only string-object pairs in context arrays are supported",
        )),
    }
}

fn check_context_prefix<E: de::Error>(prefix: &str) -> Result<(), E> {
    if prefix.contains(':') {
        Err(de::Error::custom(
            "named JSON-LD contexts in the form name1:name2 are still unsupported",
        ))
    } else {
        Ok(())
    }
}

impl Serialize for Context {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if self.named.is_empty() {
            match &self.default {
                Some(default) => default.serialize(serializer),
                None => serializer.serialize_seq(Some(0))?.end(),
            }
        } else {
            match &self.default {
                Some(default) => {
                    let mut seq = serializer.serialize_seq(Some(2))?;
                    seq.serialize_element(default)?;
                    seq.serialize_element(&self.named)?;
                    seq.end()
                }
                None => self.named.serialize(serializer),
            }
        }
    }
}

impl TryFrom<String> for Context {
    type Error = InvalidEmptyIri;

    #[inline]
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl TryFrom<&str> for Context {
    type Error = InvalidEmptyIri;

    #[inline]
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl ContextEntry {
    #[inline]
    pub fn new(iri: impl Into<String>) -> Result<Self, InvalidEmptyIri> {
        Iri::new(iri).map(Self::Iri)
    }

    #[inline]
    pub fn iri(&self) -> Option<&Iri> {
        match self {
            Self::Iri(iri) => Some(iri),
            Self::Object(obj) => obj.iri(),
        }
    }
}

impl ContextEntryObject {
    #[inline]
    pub fn iri(&self) -> Option<&Iri> {
        self.iri.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn serialize_iri() {
        assert_eq!(
            serde_json::to_string(&Iri("myiri".into())).unwrap(),
            "\"myiri\"",
        );
    }

    #[test]
    fn deserialize_valid_iri() {
        let iri: Iri = serde_json::from_value(json!("myiri")).unwrap();
        assert_eq!(iri, Iri("myiri".into()));
    }

    #[test]
    fn deserialize_empty_iri() {
        assert!(serde_json::from_value::<Iri>(json!("")).is_err());
    }

    #[test]
    fn deserialize_context_iri() {
        let context: Context = serde_json::from_value(json!("myiri")).unwrap();
        assert_eq!(
            context,
            Context {
                default: Some(ContextEntry::Iri(Iri("myiri".into()))),
                named: HashMap::new(),
            }
        );
    }

    #[test]
    fn deserialize_named_iri() {
        let context: Context = serde_json::from_value(json!({"prx": "myiri"})).unwrap();
        assert_eq!(
            context,
            Context {
                default: None,
                named: [("prx".into(), ContextEntry::Iri(Iri("myiri".into())))]
                    .into_iter()
                    .collect(),
            }
        );
    }

    #[test]
    fn deserialize_named_object_without_iri() {
        let context: Context = serde_json::from_value(json!({"prx": {"hello": "world"}})).unwrap();
        assert_eq!(
            context,
            Context {
                default: None,
                named: [(
                    "prx".into(),
                    ContextEntry::Object(ContextEntryObject {
                        iri: None,
                        other: [("hello".to_string(), json!("world"))]
                            .into_iter()
                            .collect()
                    })
                )]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn deserialize_named_object_with_iri() {
        let context: Context = serde_json::from_value(json!({
                "prx": {
                    "hello": "world",
                    "@id": "iri",
                },
            }
        ))
        .unwrap();
        assert_eq!(
            context,
            Context {
                default: None,
                named: [(
                    "prx".into(),
                    ContextEntry::Object(ContextEntryObject {
                        iri: Some(Iri("iri".into())),
                        other: [("hello".to_string(), json!("world"))]
                            .into_iter()
                            .collect()
                    })
                )]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn deserialize_full() {
        let context: Context = serde_json::from_value(json!([
            "default_iri",
            {
                "prx1": {
                    "hello": "world",
                    "@id": "iri1",
                },
                "prx2": "iri2",
            }
        ]))
        .unwrap();

        assert_eq!(
            context,
            Context {
                default: Some(ContextEntry::Iri(Iri("default_iri".into()))),
                named: [
                    (
                        "prx1".into(),
                        ContextEntry::Object(ContextEntryObject {
                            iri: Some(Iri("iri1".into())),
                            other: [("hello".to_string(), json!("world"))]
                                .into_iter()
                                .collect()
                        })
                    ),
                    ("prx2".into(), ContextEntry::Iri(Iri("iri2".into())))
                ]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn deserialize_default_string_before_at_data() {
        let context: Context = serde_json::from_value(json!([
            "default_context",
            {
                "prx1": "iri1",
                "@type": "@id",
            },
            {
                "prx2": "iri2",
            }
        ]))
        .unwrap();

        assert_eq!(
            context,
            Context {
                default: Some(ContextEntry::Object(ContextEntryObject {
                    iri: Some(Iri("default_context".into())),
                    other: [("@type".into(), "@id".into())].into_iter().collect()
                })),
                named: [
                    ("prx1".into(), ContextEntry::Iri(Iri("iri1".into()))),
                    ("prx2".into(), ContextEntry::Iri(Iri("iri2".into())))
                ]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn deserialize_default_string_after_at_data() {
        let context: Context = serde_json::from_value(json!([
            {
                "prx1": "iri1",
                "@type": "@id",
            },
            "default_context",
            {
                "prx2": "iri2",
            }
        ]))
        .unwrap();

        assert_eq!(
            context,
            Context {
                default: Some(ContextEntry::Object(ContextEntryObject {
                    iri: Some(Iri("default_context".into())),
                    other: [("@type".into(), "@id".into())].into_iter().collect(),
                })),
                named: [
                    ("prx1".into(), ContextEntry::Iri(Iri("iri1".into()))),
                    ("prx2".into(), ContextEntry::Iri(Iri("iri2".into())))
                ]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn deserialize_id_after_at_data() {
        let context: Context = serde_json::from_value(json!([
            {
                "prx1": "iri1",
                "@type": "@id",
            },
            {
                "@id": "default_context",
            },
            {
                "prx2": "iri2",
            }
        ]))
        .unwrap();

        assert_eq!(
            context,
            Context {
                default: Some(ContextEntry::Object(ContextEntryObject {
                    iri: Some(Iri("default_context".into())),
                    other: [("@type".into(), "@id".into())].into_iter().collect()
                })),
                named: [
                    ("prx1".into(), ContextEntry::Iri(Iri("iri1".into()))),
                    ("prx2".into(), ContextEntry::Iri(Iri("iri2".into()))),
                ]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn deserialize_default_using_object() {
        let context: Context = serde_json::from_value(json!([
            {
                "@id": "default_context",
                "@type": "@id",
            },
            {
                "prx1": {
                    "hello": "world",
                    "@id": "iri1",
                },
                "prx2": "iri2",
            }
        ]))
        .unwrap();

        assert_eq!(
            context,
            Context {
                default: Some(ContextEntry::Object(ContextEntryObject {
                    iri: Some(Iri("default_context".into())),
                    other: [("@type".into(), "@id".into())].into_iter().collect()
                })),
                named: [
                    (
                        "prx1".into(),
                        ContextEntry::Object(ContextEntryObject {
                            iri: Some(Iri("iri1".into())),
                            other: [("hello".to_string(), json!("world"))]
                                .into_iter()
                                .collect()
                        })
                    ),
                    ("prx2".into(), ContextEntry::Iri(Iri("iri2".into())))
                ]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn deserialize_empty() {
        assert_eq!(
            serde_json::from_value::<Context>(json!([])).unwrap(),
            Context::empty()
        );
        assert_eq!(
            serde_json::from_value::<Context>(json!({})).unwrap(),
            Context::empty()
        );
    }

    #[test]
    fn deserialize_invalid_prefix() {
        assert!(serde_json::from_value::<Context>(json!([
            "default",
            {
                "prefix1:prefix2": "iri",
            }
        ]))
        .is_err());

        assert!(serde_json::from_value::<Context>(json!({
            "prefix1:prefix2": "iri",
        }))
        .is_err());

        assert!(serde_json::from_value::<Context>(json!([
            "default",
            {
                "prefix1:prefix2": {
                    "@id": "iri",
                    "@type": "@id",
                }
            }
        ]))
        .is_err());

        assert!(serde_json::from_value::<Context>(json!({
            "prefix1:prefix2": {
                "@id": "iri",
                "@type": "@id",
            }
        }))
        .is_err());
    }

    #[test]
    fn deserialize_invalid_empty_iri() {
        assert!(serde_json::from_value::<Context>(json!("")).is_err());
        assert!(serde_json::from_value::<Context>(json!({
            "pfx": "",
        }))
        .is_err());
        assert!(serde_json::from_value::<Context>(json!([
            "",
            {
                "pfx": "iri",
            }
        ]))
        .is_err());
        assert!(serde_json::from_value::<Context>(json!([
            "default",
            {
                "pfx": "",
            }
        ]))
        .is_err());
        assert!(serde_json::from_value::<Context>(json!({
            "@id": "",
        }))
        .is_err());
        assert!(serde_json::from_value::<Context>(json!({
            "@id": "",
        }))
        .is_err());
        assert!(serde_json::from_value::<Context>(json!({
            "pfx": {
                "@id": "",
            },
        }))
        .is_err());
        assert!(serde_json::from_value::<Context>(json!([
            "default",
            {
                "pfx": {
                    "@id": "",
                },
            }
        ]))
        .is_err());
    }

    #[test]
    fn deserialize_more_than_one_default_context() {
        assert!(serde_json::from_value::<Context>(json!(["iri1", "iri2"])).is_err());
        assert!(serde_json::from_value::<Context>(json!(["iri1", { "@id": "iri2" }])).is_err());
    }

    #[test]
    fn serialize_only_default() {
        let context = Context {
            default: Some(ContextEntry::new("iri").unwrap()),
            named: HashMap::new(),
        };
        assert_eq!(serde_json::to_string(&context).unwrap(), r#""iri""#);
    }

    #[test]
    fn serialize_only_named() {
        let context = Context {
            default: None,
            named: [
                ("a".into(), ContextEntry::new("iri1").unwrap()),
                ("b".into(), ContextEntry::new("iri2").unwrap()),
            ]
            .into_iter()
            .collect(),
        };

        let serialized = serde_json::to_string(&context).unwrap();
        assert!(
            serialized == r#"{"a":"iri1","b":"iri2"}"#
                || serialized == r#"{"b":"iri2","a":"iri1"}"#
        );
    }

    #[test]
    fn serialize_default_and_named() {
        let context = Context {
            default: Some(ContextEntry::new("default").unwrap()),
            named: [
                ("a".into(), ContextEntry::new("iri1").unwrap()),
                ("b".into(), ContextEntry::new("iri2").unwrap()),
            ]
            .into_iter()
            .collect(),
        };
        let serialized = serde_json::to_string(&context).unwrap();
        assert!(
            serialized == r#"["default",{"a":"iri1","b":"iri2"}]"#
                || serialized == r#"["default",{"b":"iri2","a":"iri1"}]"#
        );
    }

    #[test]
    fn serialize_nothing() {
        let context = Context {
            default: None,
            named: HashMap::new(),
        };
        assert_eq!(serde_json::to_string(&context).unwrap(), r#"[]"#);
    }

    #[test]
    fn serialize_context_entry_iri() {
        let entry = ContextEntry::Iri(Iri("iri".into()));
        assert_eq!(serde_json::to_string(&entry).unwrap(), r#""iri""#);
    }

    #[test]
    fn serialize_context_entry_object_with_iri() {
        let entry = ContextEntry::Object(ContextEntryObject {
            iri: Some(Iri("iri".to_string())),
            other: [("@type".into(), "@id".into())].into_iter().collect(),
        });
        assert_eq!(
            serde_json::to_string(&entry).unwrap(),
            r#"{"@id":"iri","@type":"@id"}"#
        );
    }

    #[test]
    fn serialize_context_entry_object_without_iri() {
        let entry = ContextEntry::Object(ContextEntryObject {
            iri: None,
            other: [("@type".into(), "@id".into())].into_iter().collect(),
        });
        assert_eq!(serde_json::to_string(&entry).unwrap(), r#"{"@type":"@id"}"#);
    }
}
