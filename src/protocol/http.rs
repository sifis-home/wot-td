//! HTTP Binding Template

use std::{borrow::Cow, fmt};

use crate::{
    extend::{ExtendableThing, ThingContext},
    seedable::{make_seed, OptionSeed, Seed, Seedable},
};
use serde::{
    de::{self, DeserializeSeed, VariantAccess, Visitor},
    Deserialize, Deserializer, Serialize,
};
use serde_with::{serde_as, skip_serializing_none};

/// HTTP request method
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Method {
    Get,
    Put,
    Post,
    Delete,
    Patch,
}

/// HTTP Header
#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash, Default)]
pub struct MessageHeader {
    #[serde(rename = "htv:fieldName")]
    pub field_name: Option<String>,
    #[serde(rename = "htv:fieldValue")]
    pub field_value: Option<String>,
}

/// Extended fields for ExpectedResponse and AdditionalResponse
#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, Default, PartialEq, Eq, Hash)]
pub struct Response {
    #[serde(rename = "htv:headers")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub headers: Vec<MessageHeader>,
    #[serde(rename = "htv:statusCodeValue")]
    pub status_code_value: Option<usize>,
}

/// Extended fields for Form
#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, Default, PartialEq, Eq, Hash)]
pub struct Form {
    #[serde(rename = "htv:methodName")]
    pub method_name: Option<Method>,
}

/// HTTP Protocol extension
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash, Default)]
pub struct HttpProtocol {}

impl ExtendableThing for HttpProtocol {
    type InteractionAffordance = ();
    type PropertyAffordance = ();
    type ActionAffordance = ();
    type EventAffordance = ();
    type Form = Form;
    type ExpectedResponse = Response;
    type DataSchema = ();
    type ObjectSchema = ();
    type ArraySchema = ();
}

make_seed!(
    Response => ResponseSeed,
    MessageHeader => MessageHeaderSeed,
    Form => FormSeed,
    Method => MethodSeed,
    HttpProtocol => HttpProtocolSeed,
);

macro_rules! impl_thing_context {
    ($( $ty:ty ),+ $(,)?) => {
        $(
            impl ThingContext for $ty {
                const CONTEXT_MAP: &'static [[&'static str; 2]] = &[["htv", "http://www.w3.org/2011/http"]];
            }
        )+
    };
}

impl_thing_context!(Response, MessageHeader, Form, Method, HttpProtocol);

impl<'a, 'b, 'de> DeserializeSeed<'de> for ResponseSeed<'a, 'b, 'de> {
    type Value = Response;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Field<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        enum FieldName {
            Headers,
            StatusCodeValue,
        }

        const FIELDS: &[&str] = &["headers", "statusCodeValue"];

        impl<'a, 'b, 'de> DeserializeSeed<'de> for Field<'a, 'b, 'de> {
            type Value = FieldName;

            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct FieldVisitor<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

                impl<'a, 'b, 'de> Visitor<'de> for FieldVisitor<'a, 'b, 'de> {
                    type Value = FieldName;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str(
                            "a prefix related to the IRI http://www.w3.org/2011/http followed by \
                             one of the identifiers: headers, statusCodeValue",
                        )
                    }

                    #[inline]
                    fn visit_borrowed_str<E>(mut self, v: &'de str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        let field = v.split_once(':').map(move |(prefix, field)| {
                            let &mut Seed {
                                cell,
                                ref mut owner,
                            } = &mut self.0;
                            let map = cell.rw(owner);

                            let mapped_prefix = map
                                .iter()
                                .find(|(cur_prefix, _)| cur_prefix.as_ref() == prefix);
                            if mapped_prefix.is_none() {
                                map.push((Cow::Borrowed(prefix), "http://www.w3.org/2011/http"));
                            }

                            field
                        });

                        match field {
                            Some("headers") => Ok(FieldName::Headers),
                            Some("statusCodeValue") => Ok(FieldName::StatusCodeValue),
                            Some(field) => Err(de::Error::unknown_field(field, FIELDS)),
                            None => Err(de::Error::custom("expected prefix")),
                        }
                    }

                    #[inline]
                    fn visit_str<E>(mut self, v: &str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        let field = v.split_once(':').map(move |(prefix, field)| {
                            let &mut Seed {
                                cell,
                                ref mut owner,
                            } = &mut self.0;
                            let map = cell.rw(owner);

                            let mapped_prefix = map
                                .iter()
                                .find(|(cur_prefix, _)| cur_prefix.as_ref() == prefix);
                            if mapped_prefix.is_none() {
                                map.push((
                                    Cow::Owned(prefix.to_owned()),
                                    "http://www.w3.org/2011/http",
                                ));
                            }

                            field
                        });

                        match field {
                            Some("headers") => Ok(FieldName::Headers),
                            Some("statusCodeValue") => Ok(FieldName::StatusCodeValue),
                            Some(field) => Err(de::Error::unknown_field(field, FIELDS)),
                            None => Err(de::Error::custom("expected prefix")),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor(self.0))
            }
        }

        struct Visit<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        impl<'a, 'b, 'de> Visitor<'de> for Visit<'a, 'b, 'de> {
            type Value = Response;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map of elements")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut headers = None;
                let mut status_code_value = None;

                let Seed { cell, owner } = self.0;

                while let Some(field) = map.next_key_seed(Field(Seed {
                    cell,
                    owner: &mut *owner,
                }))? {
                    match field {
                        FieldName::Headers => {
                            if headers.is_some() {
                                return Err(de::Error::duplicate_field("headers"));
                            }

                            let seed = <Vec<MessageHeader> as Seedable>::new_seed(Seed {
                                cell,
                                owner: &mut *owner,
                            });
                            let next_value = map.next_value_seed(seed)?;
                            headers = Some(next_value);
                        }
                        FieldName::StatusCodeValue => {
                            if status_code_value.is_some() {
                                return Err(de::Error::duplicate_field("statusCodeValue"));
                            }

                            let seed = <Option<usize> as Seedable>::new_seed(Seed {
                                cell,
                                owner: &mut *owner,
                            });
                            status_code_value = Some(map.next_value_seed(seed)?);
                        }
                    }
                }

                let Some(headers) = headers else {
                    return Err(de::Error::missing_field("headers"));
                };
                let status_code_value = status_code_value.flatten();

                Ok(Response {
                    headers,
                    status_code_value,
                })
            }
        }

        deserializer.deserialize_map(Visit(self.0))
    }
}

impl<'a, 'b, 'de> DeserializeSeed<'de> for MessageHeaderSeed<'a, 'b, 'de> {
    type Value = MessageHeader;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Field<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        enum FieldName {
            FieldName,
            FieldValue,
        }

        const FIELDS: &[&str] = &["fieldName", "fieldValue"];

        impl<'a, 'b, 'de> DeserializeSeed<'de> for Field<'a, 'b, 'de> {
            type Value = FieldName;

            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct Visit<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

                impl<'a, 'b, 'de> Visitor<'de> for Visit<'a, 'b, 'de> {
                    type Value = FieldName;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str(
                            "a prefix related to the IRI http://www.w3.org/2011/http followed by \
                             one of the identifiers: fieldName, fieldValue",
                        )
                    }

                    #[inline]
                    fn visit_borrowed_str<E>(mut self, v: &'de str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        let field = v.split_once(':').map(move |(prefix, field)| {
                            let &mut Seed {
                                cell,
                                ref mut owner,
                            } = &mut self.0;
                            let map = cell.rw(owner);

                            let mapped_prefix = map
                                .iter()
                                .find(|(cur_prefix, _)| cur_prefix.as_ref() == prefix);
                            if mapped_prefix.is_none() {
                                map.push((Cow::Borrowed(prefix), "http://www.w3.org/2011/http"));
                            }

                            field
                        });

                        match field {
                            Some("fieldName") => Ok(FieldName::FieldName),
                            Some("fieldValue") => Ok(FieldName::FieldValue),
                            Some(field) => Err(de::Error::unknown_field(field, FIELDS)),
                            None => Err(de::Error::custom("expected prefix")),
                        }
                    }

                    #[inline]
                    fn visit_str<E>(mut self, v: &str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        let field = v.split_once(':').map(move |(prefix, field)| {
                            let &mut Seed {
                                cell,
                                ref mut owner,
                            } = &mut self.0;
                            let map = cell.rw(owner);

                            let mapped_prefix = map
                                .iter()
                                .find(|(cur_prefix, _)| cur_prefix.as_ref() == prefix);
                            if mapped_prefix.is_none() {
                                map.push((
                                    Cow::Owned(prefix.to_owned()),
                                    "http://www.w3.org/2011/http",
                                ));
                            }

                            field
                        });

                        match field {
                            Some("fieldName") => Ok(FieldName::FieldName),
                            Some("fieldValue") => Ok(FieldName::FieldValue),
                            Some(field) => Err(de::Error::unknown_field(field, FIELDS)),
                            None => Err(de::Error::custom("expected prefix")),
                        }
                    }
                }

                deserializer.deserialize_str(Visit(self.0))
            }
        }

        struct Visit<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        impl<'a, 'b, 'de> Visitor<'de> for Visit<'a, 'b, 'de> {
            type Value = MessageHeader;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map of elements")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut field_name = None;
                let mut field_value = None;

                let Seed { cell, owner } = self.0;

                while let Some(field) = map.next_key_seed(Field(Seed {
                    cell,
                    owner: &mut *owner,
                }))? {
                    match field {
                        FieldName::FieldName => {
                            if field_name.is_some() {
                                return Err(de::Error::duplicate_field("fieldName"));
                            }

                            let seed: OptionSeed<'a, '_, 'de, _> =
                                <Option<String> as Seedable>::new_seed(Seed {
                                    cell,
                                    owner: &mut *owner,
                                });
                            field_name = Some(map.next_value_seed(seed)?);
                        }
                        FieldName::FieldValue => {
                            if field_value.is_some() {
                                return Err(de::Error::duplicate_field("fieldValue"));
                            }

                            let seed = <Option<String> as Seedable>::new_seed(Seed {
                                cell,
                                owner: &mut *owner,
                            });
                            field_value = Some(map.next_value_seed(seed)?);
                        }
                    }
                }

                let field_name = field_name.flatten();
                let field_value = field_value.flatten();

                Ok(MessageHeader {
                    field_name,
                    field_value,
                })
            }
        }

        deserializer.deserialize_map(Visit(self.0))
    }
}

impl<'a, 'b, 'de> DeserializeSeed<'de> for FormSeed<'a, 'b, 'de> {
    type Value = Form;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum Field {
            MethodName,
        }

        struct FieldSeed<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        struct FieldSeedVisitor<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        impl<'a, 'b, 'de> Visitor<'de> for FieldSeedVisitor<'a, 'b, 'de> {
            type Value = Field;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str(
                    "a prefix related to the IRI http://www.w3.org/2011/http followed by one of the\
                     identifiers: methodName",
                )
            }

            #[inline]
            fn visit_borrowed_str<E>(mut self, v: &'de str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let field = v.split_once(':').map(move |(prefix, field)| {
                    let &mut Seed {
                        cell,
                        ref mut owner,
                    } = &mut self.0;
                    let map = cell.rw(owner);

                    let mapped_prefix = map
                        .iter()
                        .find(|(cur_prefix, _)| cur_prefix.as_ref() == prefix);
                    if mapped_prefix.is_none() {
                        map.push((Cow::Borrowed(prefix), "http://www.w3.org/2011/http"));
                    }

                    field
                });

                match field {
                    Some("methodName") => Ok(Field::MethodName),
                    Some(field) => Err(de::Error::unknown_field(field, FIELDS)),
                    None => Err(de::Error::custom("expected prefix")),
                }
            }

            #[inline]
            fn visit_str<E>(mut self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let field = v.split_once(':').map(move |(prefix, field)| {
                    let &mut Seed {
                        cell,
                        ref mut owner,
                    } = &mut self.0;
                    let map = cell.rw(owner);

                    let mapped_prefix = map
                        .iter()
                        .find(|(cur_prefix, _)| cur_prefix.as_ref() == prefix);
                    if mapped_prefix.is_none() {
                        map.push((Cow::Owned(prefix.to_owned()), "http://www.w3.org/2011/http"));
                    }

                    field
                });

                match field {
                    Some("methodName") => Ok(Field::MethodName),
                    Some(field) => Err(de::Error::unknown_field(field, FIELDS)),
                    None => Err(de::Error::custom("expected prefix")),
                }
            }

            fn visit_borrowed_bytes<E>(self, v: &'de [u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Seed { cell, owner } = self.0;
                let map = cell.rw(owner);

                let mut split = v.splitn(2, |&c| c == b':');
                let pair = split
                    .next()
                    .and_then(|prefix| std::str::from_utf8(prefix).ok())
                    .zip(split.next());

                match pair {
                    Some((prefix, field)) => {
                        let mapped_prefix = map.iter().find(|(cur_prefix, _)| cur_prefix == prefix);
                        if mapped_prefix.is_none() {
                            map.push((Cow::Borrowed(prefix), "http://www.w3.org/2011/http"));
                        }

                        match field {
                            b"methodName" => Ok(Field::MethodName),
                            _ => Err(de::Error::unknown_field(
                                &String::from_utf8_lossy(field),
                                FIELDS,
                            )),
                        }
                    }
                    None => Err(de::Error::custom("expected prefix")),
                }
            }
        }

        impl<'a, 'b, 'de> DeserializeSeed<'de> for FieldSeed<'a, 'b, 'de> {
            type Value = Field;

            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_identifier(FieldSeedVisitor(self.0))
            }
        }

        struct FormVisitor<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        impl<'a, 'b, 'de> Visitor<'de> for FormVisitor<'a, 'b, 'de> {
            type Value = Form;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map of elements")
            }

            fn visit_map<A>(mut self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut method_name = None;

                while let Some(field) = map.next_key_seed(FieldSeed(self.0.reseed()))? {
                    match field {
                        Field::MethodName => {
                            if method_name.is_some() {
                                return Err(de::Error::duplicate_field("methodName"));
                            }

                            let seed: OptionSeed<'a, '_, 'de, _> =
                                <Option<Method> as Seedable>::new_seed(self.0.reseed());
                            method_name = Some(map.next_value_seed(seed)?);
                        }
                    }
                }

                let method_name = method_name.flatten();

                Ok(Form { method_name })
            }
        }

        const FIELDS: &[&str] = &["methodName"];
        deserializer.deserialize_struct("Form", FIELDS, FormVisitor(self.0))
    }
}

impl<'a, 'b, 'de> DeserializeSeed<'de> for MethodSeed<'a, 'b, 'de> {
    type Value = Method;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        const VARIANTS: &[&str] = &["GET", "PUT", "POST", "DELETE", "PATCH"];
        enum Field {
            Get,
            Put,
            Post,
            Delete,
            Patch,
        }
        struct FieldSeed<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        struct FieldVisitor<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        impl<'a, 'b, 'de> Visitor<'de> for FieldVisitor<'a, 'b, 'de> {
            type Value = Field;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("variant identifier")
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    0 => Ok(Field::Get),
                    1 => Ok(Field::Put),
                    2 => Ok(Field::Post),
                    3 => Ok(Field::Delete),
                    4 => Ok(Field::Patch),
                    _ => Err(de::Error::invalid_value(
                        de::Unexpected::Unsigned(v),
                        &"variant index 0 <= i < 5",
                    )),
                }
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "GET" => Ok(Field::Get),
                    "PUT" => Ok(Field::Put),
                    "POST" => Ok(Field::Post),
                    "DELETE" => Ok(Field::Delete),
                    "PATCH" => Ok(Field::Patch),
                    _ => Err(de::Error::unknown_variant(v, VARIANTS)),
                }
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    b"GET" => Ok(Field::Get),
                    b"PUT" => Ok(Field::Put),
                    b"POST" => Ok(Field::Post),
                    b"DELETE" => Ok(Field::Delete),
                    b"PATCH" => Ok(Field::Patch),
                    _ => Err(de::Error::unknown_variant(
                        &String::from_utf8_lossy(v),
                        VARIANTS,
                    )),
                }
            }
        }

        impl<'a, 'b, 'de> DeserializeSeed<'de> for FieldSeed<'a, 'b, 'de> {
            type Value = Field;

            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_identifier(FieldVisitor(self.0))
            }
        }

        struct MethodVisitor<'a, 'b, 'de>(Seed<'a, 'b, 'de>);
        impl<'a, 'b, 'de> Visitor<'de> for MethodVisitor<'a, 'b, 'de> {
            type Value = Method;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("enum Method")
            }

            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: de::EnumAccess<'de>,
            {
                match data.variant_seed(FieldSeed(self.0))? {
                    (Field::Get, variant) => {
                        variant.unit_variant()?;
                        Ok(Method::Get)
                    }
                    (Field::Put, variant) => {
                        variant.unit_variant()?;
                        Ok(Method::Put)
                    }
                    (Field::Post, variant) => {
                        variant.unit_variant()?;
                        Ok(Method::Post)
                    }
                    (Field::Delete, variant) => {
                        variant.unit_variant()?;
                        Ok(Method::Delete)
                    }
                    (Field::Patch, variant) => {
                        variant.unit_variant()?;
                        Ok(Method::Patch)
                    }
                }
            }
        }

        deserializer.deserialize_enum("Method", VARIANTS, MethodVisitor(self.0))
    }
}

#[allow(clippy::match_single_binding)]
impl<'a, 'b, 'de> DeserializeSeed<'de> for HttpProtocolSeed<'a, 'b, 'de> {
    type Value = HttpProtocol;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum HttpProtocolField {
            Ignore,
        }

        struct HttpProtocolFieldSeed<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        struct HttpProtocolFieldVisitor<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        impl<'a, 'b, 'de> Visitor<'de> for HttpProtocolFieldVisitor<'a, 'b, 'de> {
            type Value = HttpProtocolField;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("field identifier")
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    _ => Ok(HttpProtocolField::Ignore),
                }
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    _ => Ok(HttpProtocolField::Ignore),
                }
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    _ => Ok(HttpProtocolField::Ignore),
                }
            }
        }

        impl<'a, 'b, 'de> DeserializeSeed<'de> for HttpProtocolFieldSeed<'a, 'b, 'de> {
            type Value = HttpProtocolField;

            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_identifier(HttpProtocolFieldVisitor(self.0))
            }
        }

        struct HttpProtocolVisitor<'a, 'b, 'de>(Seed<'a, 'b, 'de>);

        impl<'a, 'b, 'de> Visitor<'de> for HttpProtocolVisitor<'a, 'b, 'de> {
            type Value = HttpProtocol;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct HttpProtocol")
            }

            fn visit_seq<A>(self, _seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                Ok(HttpProtocol {})
            }

            fn visit_map<A>(mut self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                while let Some(key) = map.next_key_seed(HttpProtocolFieldSeed(self.0.reseed()))? {
                    match key {
                        _ => {
                            let _ = map.next_value::<de::IgnoredAny>()?;
                        }
                    }
                }

                Ok(HttpProtocol {})
            }
        }

        const FIELDS: &[&str] = &[];
        deserializer.deserialize_struct("HttpProtocol", FIELDS, HttpProtocolVisitor(self.0))
    }
}

#[cfg(test)]
mod test {
    use serde::de::IntoDeserializer;
    use serde_json::json;

    use super::*;
    use crate::{
        seedable::{self, Contexts, SeedCell, SeedOwner},
        thing::ExpectedResponse,
    };

    fn deserialize_form(s: &str, r: crate::thing::Form<HttpProtocol>) {
        let f: crate::thing::Form<HttpProtocol> = serde_json::from_str(s).unwrap();
        assert_eq!(f, r);
    }

    #[test]
    fn deserialize_discovery_property() {
        let property = r#"
        {
            "href": "/things{?offset,limit,format,sort_by,sort_order}",
            "htv:methodName": "GET",
            "response": {
                "description": "Success response",
                "htv:statusCodeValue": 200,
                "contentType": "application/ld+json",
                "htv:headers": [
                    {
                        "htv:fieldName": "Link"
                    }
                ]
            }
        }
        "#;

        let expected = crate::thing::Form {
            href: "/things{?offset,limit,format,sort_by,sort_order}".into(),
            response: Some(ExpectedResponse {
                content_type: "application/ld+json".into(),
                other: Response {
                    headers: vec![MessageHeader {
                        field_name: Some("Link".into()),
                        field_value: None,
                    }],
                    status_code_value: Some(200),
                },
            }),
            other: Form {
                method_name: Some(Method::Get),
            },
            ..Default::default()
        };

        deserialize_form(property, expected);
    }

    #[test]
    fn deserialize_discovery_action() {
        let action = r#"
        {
            "href": "/things",
            "htv:methodName": "POST",
            "response": {
                "contentType": "application/td+json",
                "description": "Success response including the system-generated URI",
                "htv:headers": [
                    {
                        "description": "System-generated URI",
                        "htv:fieldName": "Location"
                    }
                ],
                "htv:statusCodeValue": 201
            }
        }
        "#;

        let expected = crate::thing::Form {
            op: Default::default(),
            href: "/things".into(),
            response: Some(ExpectedResponse {
                content_type: "application/td+json".into(),
                other: Response {
                    headers: vec![MessageHeader {
                        field_name: Some("Location".into()),
                        field_value: None,
                    }],
                    status_code_value: Some(201),
                },
            }),
            other: Form {
                method_name: Some(Method::Post),
            },
            ..Default::default()
        };

        deserialize_form(action, expected);
    }

    #[test]
    fn deserialize_seed_simple() {
        let data = json!({
            "href": "https://href",
            "htv:methodName": "PATCH",
            "response": {
                "contentType": "application/json",
                "htv:headers": [
                    {
                        "htv:fieldName": "field1",
                        "htv:fieldValue": "value1",
                    },
                    {
                        "htv:fieldName": "field2",
                        "htv:fieldValue": "value2",
                    },
                ],
                "htv:statusCodeValue": 200,
            },
        });

        let (deserialized, contexts) =
            seedable::deserialize!(data, crate::thing::Form::<HttpProtocol>).unwrap();

        let expected_data = crate::thing::Form {
            href: "https://href".into(),
            other: Form {
                method_name: Some(Method::Patch),
            },
            response: Some(ExpectedResponse {
                content_type: "application/json".to_owned(),
                other: Response {
                    headers: vec![
                        MessageHeader {
                            field_name: Some("field1".to_owned()),
                            field_value: Some("value1".to_owned()),
                        },
                        MessageHeader {
                            field_name: Some("field2".to_owned()),
                            field_value: Some("value2".to_owned()),
                        },
                    ],
                    status_code_value: Some(200),
                },
            }),
            ..Default::default()
        };

        assert_eq!(deserialized, expected_data);
        assert_eq!(contexts, &[(Cow::Borrowed("htv"), "https://href")]);
    }
}
