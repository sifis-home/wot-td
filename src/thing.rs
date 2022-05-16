use std::{borrow::Cow, collections::HashMap};

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;
use serde_with::{serde_as, skip_serializing_none, DeserializeAs, OneOrMany, Same};
use time::OffsetDateTime;

type MultiLanguage = HashMap<String, String>;
type DataSchemaMap = HashMap<String, DataSchema>;

const TD_CONTEXT: &str = "https://www.w3.org/2019/wot/td/v1";

/// Connected thing
#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Thing {
    // The context can be arbitrarily complex
    // https://www.w3.org/TR/json-ld11/#the-context
    // Let's take a value for now and assume we'll use the json-ld crate later
    #[serde(rename = "@context", default = "Thing::default_context")]
    pub context: Value,

    pub id: Option<String>,

    #[serde(rename = "@type", default)]
    #[serde_as(deserialize_as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub title: String,

    pub titles: Option<MultiLanguage>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    pub version: Option<VersionInfo>,

    #[serde(with = "time::serde::rfc3339::option", default)]
    pub created: Option<OffsetDateTime>,

    #[serde(with = "time::serde::rfc3339::option", default)]
    pub modified: Option<OffsetDateTime>,

    // FIXME: use AnyURI
    pub support: Option<String>,

    // FIXME: use AnyURI
    pub base: Option<String>,

    pub properties: Option<HashMap<String, PropertyAffordance>>,

    pub actions: Option<HashMap<String, ActionAffordance>>,

    pub events: Option<HashMap<String, EventAffordance>>,

    pub links: Option<Vec<Link>>,

    pub forms: Option<Vec<Form>>,

    #[serde_as(deserialize_as = "OneOrMany<_>")]
    pub security: Vec<String>,

    pub security_definitions: HashMap<String, SecurityScheme>,
}

impl Thing {
    fn default_context() -> Value {
        TD_CONTEXT.into()
    }
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InteractionAffordance {
    #[serde(rename = "@type", default)]
    #[serde_as(deserialize_as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub title: Option<String>,

    pub titles: Option<MultiLanguage>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    pub forms: Vec<Form>,

    pub uri_variables: Option<DataSchemaMap>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct PropertyAffordance {
    #[serde(flatten)]
    pub interaction: InteractionAffordance,

    #[serde(flatten)]
    pub data_schema: DataSchema,

    pub observable: Option<bool>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct ActionAffordance {
    #[serde(flatten)]
    pub interaction: InteractionAffordance,

    pub input: Option<DataSchema>,

    pub output: Option<DataSchema>,

    #[serde(default)]
    pub safe: bool,

    #[serde(default)]
    pub idempotent: bool,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct EventAffordance {
    #[serde(flatten)]
    pub interaction: InteractionAffordance,

    pub subscription: Option<DataSchema>,

    pub data: Option<DataSchema>,

    pub cancellation: Option<DataSchema>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct VersionInfo {
    instance: String,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DataSchema {
    #[serde(rename = "@type")]
    pub attype: Option<String>,

    pub title: Option<String>,

    pub titles: Option<MultiLanguage>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    #[serde(rename = "const")]
    pub constant: Option<Value>,

    pub unit: Option<String>,

    pub one_of: Option<Vec<DataSchema>>,

    #[serde(rename = "enum")]
    pub enumeration: Option<Vec<Value>>,

    #[serde(default)]
    pub read_only: bool,

    #[serde(default)]
    pub write_only: bool,

    pub format: Option<String>,

    #[serde(flatten)]
    pub subtype: DataSchemaSubtype,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum DataSchemaSubtype {
    Array(ArraySchema),
    Boolean,
    Number(NumberSchema),
    Integer(IntegerSchema),
    Object(ObjectSchema),
    String,
    Null,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct ArraySchema {
    pub min_items: Option<u32>,

    pub max_items: Option<u32>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct NumberSchema {
    pub maximum: Option<f64>,

    pub minimum: Option<f64>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
// FIXME: we should probably use a Decimal type
pub struct IntegerSchema {
    pub maximum: Option<usize>,

    pub minimum: Option<usize>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct ObjectSchema {
    pub properties: Option<DataSchemaMap>,

    pub required: Option<Vec<String>>,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct SecurityScheme {
    #[serde(rename = "@type", default)]
    #[serde_as(deserialize_as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    // FIXME: use AnyURI
    pub proxy: Option<String>,

    #[serde(flatten)]
    pub subtype: SecuritySchemeSubtype,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(tag = "scheme", rename_all = "lowercase")]
pub enum KnownSecuritySchemeSubtype {
    NoSec,
    Basic(BasicSecurityScheme),
    Digest(DigestSecurityScheme),
    Bearer(BearerSecurityScheme),
    Psk(PskSecurityScheme),
    OAuth2(OAuth2SecurityScheme),
    ApiKey(ApiKeySecurityScheme),
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct UnknownSecuritySchemeSubtype {
    pub scheme: String,
    #[serde(flatten)]
    pub data: Value,
}

// TODO
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum SecuritySchemeSubtype {
    Known(KnownSecuritySchemeSubtype),
    Unknown(UnknownSecuritySchemeSubtype),
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct BasicSecurityScheme {
    #[serde(rename = "in", default = "SecurityAuthenticationLocation::header")]
    pub location: SecurityAuthenticationLocation,
    pub name: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SecurityAuthenticationLocation {
    Header,
    Query,
    Body,
    Cookie,
}

impl SecurityAuthenticationLocation {
    const fn header() -> Self {
        Self::Header
    }

    const fn query() -> Self {
        Self::Query
    }
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct DigestSecurityScheme {
    pub qop: QualityOfProtection,

    #[serde(rename = "in", default = "SecurityAuthenticationLocation::header")]
    pub location: SecurityAuthenticationLocation,

    pub name: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum QualityOfProtection {
    Auth,
    AuthInt,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct ApiKeySecurityScheme {
    #[serde(rename = "in", default = "SecurityAuthenticationLocation::query")]
    pub location: SecurityAuthenticationLocation,

    pub name: Option<String>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct BearerSecurityScheme {
    // FIXME: use AnyURI
    pub authorization: Option<String>,

    #[serde(default = "BearerSecurityScheme::default_alg")]
    pub alg: Cow<'static, str>,

    #[serde(default = "BearerSecurityScheme::default_format")]
    pub format: Cow<'static, str>,

    #[serde(rename = "in", default = "SecurityAuthenticationLocation::header")]
    pub location: SecurityAuthenticationLocation,

    pub name: Option<String>,
}

impl BearerSecurityScheme {
    const fn default_alg() -> Cow<'static, str> {
        Cow::Borrowed("ES256")
    }

    const fn default_format() -> Cow<'static, str> {
        Cow::Borrowed("jwt")
    }
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct PskSecurityScheme {
    pub identity: Option<String>,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct OAuth2SecurityScheme {
    // FIXME: use AnyURI
    pub authorization: Option<String>,

    // FIXME: use AnyURI
    pub token: Option<String>,

    // FIXME: use AnyURI
    pub refresh: Option<String>,

    #[serde(default)]
    #[serde_as(deserialize_as = "Option<OneOrMany<_>>")]
    pub scopes: Option<Vec<String>>,

    pub flow: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct Link {
    pub href: String,

    #[serde(rename = "type")]
    pub ty: Option<String>,

    pub rel: Option<String>,

    // FIXME: use AnyURI
    pub anchor: Option<String>,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Form {
    #[serde(default)]
    pub op: DefaultedFormOperations,

    // FIXME: use AnyURI
    pub href: String,

    #[serde(default = "Form::default_content_type")]
    pub content_type: Cow<'static, str>,

    // TODO: check if the subset of possible values is limited by the [IANA HTTP content coding
    // registry](https://www.iana.org/assignments/http-parameters/http-parameters.xhtml#content-coding).
    pub content_coding: Option<String>,

    pub subprotocol: Option<String>,

    // FIXME: use variant names of KnownSecuritySchemeSubtype + "other" string variant
    #[serde(default)]
    #[serde_as(deserialize_as = "Option<OneOrMany<_>>")]
    pub security: Option<Vec<String>>,

    #[serde(default)]
    #[serde_as(deserialize_as = "Option<OneOrMany<_>>")]
    pub scopes: Option<Vec<String>>,

    pub response: Option<ExpectedResponse>,
}

impl Form {
    const fn default_content_type() -> Cow<'static, str> {
        Cow::Borrowed("application/json")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum FormOperation {
    ReadProperty,
    WriteProperty,
    ObserveProperty,
    UnobserveProperty,
    InvokeAction,
    SubscribeEvent,
    UnsubscribeEvent,
    ReadAllProperties,
    WriteAllProperties,
    ReadMultipleProperties,
    WriteMultipleProperties,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DefaultedFormOperations {
    Default,
    Custom(Vec<FormOperation>),
}

impl Default for DefaultedFormOperations {
    fn default() -> Self {
        Self::Default
    }
}

impl Serialize for DefaultedFormOperations {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Default => serializer.serialize_none(),
            Self::Custom(ops) if ops.is_empty() => serializer.serialize_none(),
            Self::Custom(ops) => ops.serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for DefaultedFormOperations
where
    OneOrMany<Same>: DeserializeAs<'de, Vec<FormOperation>>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let ops = Option::<OneOrMany<_>>::deserialize_as(deserializer)?;
        Ok(ops.map(Self::Custom).unwrap_or(Self::Default))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExpectedResponse {
    pub content_type: String,

    #[serde(flatten)]
    pub other: Value,
}

#[cfg(test)]
mod test {
    use time::macros::datetime;

    use super::*;

    #[test]
    fn minimal_thing() {
        const RAW: &str = r#"
        {
            "@context": "https://www.w3.org/2019/wot/td/v1",
            "id": "urn:dev:ops:32473-WoTLamp-1234",
            "title": "MyLampThing",
            "securityDefinitions": {
                "nosec": {"scheme": "nosec"}
            },
            "security": ["nosec"]
        }"#;

        let expected_thing = Thing {
            context: TD_CONTEXT.into(),
            id: Some("urn:dev:ops:32473-WoTLamp-1234".to_string()),
            title: "MyLampThing".to_string(),
            security_definitions: [(
                "nosec".to_string(),
                SecurityScheme {
                    attype: None,
                    description: None,
                    descriptions: None,
                    proxy: None,
                    subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::NoSec),
                },
            )]
            .into_iter()
            .collect(),
            security: vec!["nosec".to_string()],
            attype: None,
            titles: None,
            description: None,
            descriptions: None,
            version: None,
            created: None,
            modified: None,
            support: None,
            base: None,
            properties: None,
            actions: None,
            events: None,
            links: None,
            forms: None,
        };

        let thing: Thing = serde_json::from_str(RAW).unwrap();
        assert_eq!(thing, expected_thing);

        let thing: Thing = serde_json::from_value(serde_json::to_value(thing).unwrap()).unwrap();
        assert_eq!(thing, expected_thing);
    }

    #[test]
    fn complete_thing() {
        const RAW: &str = r#"
        {
          "@context": "https://www.w3.org/2019/wot/td/v1",
          "id": "urn:dev:ops:32473-WoTLamp-1234",
          "@type": [
            "Thing",
            "LampThing"
          ],
          "title": "MyLampThing",
          "titles": {
            "en": "MyLampThing",
            "it": "La mia lampada intelligente"
          },
          "description": "A simple smart lamp",
          "descriptions": {
            "en": "A simple smart lamp",
            "it": "Una semplice lampada intelligente"
          },
          "version": {
            "instance": "0.1.0"
          },
          "created": "2022-05-01T10:20:42.123Z",
          "modified": "2022-05-10T12:30:00.000+01:00",
          "support": "mailto:mail@test.com",
          "base": "https://mylamp.example.com/",
          "properties": {
            "status": {
              "type": "string",
              "forms": [
                {
                  "href": "https://mylamp.example.com/status"
                }
              ]
            }
          },
          "actions": {
            "toggle": {
              "forms": [
                {
                  "href": "https://mylamp.example.com/toggle"
                }
              ]
            }
          },
          "events": {
            "overheating": {
              "data": {
                "type": "string"
              },
              "forms": [
                {
                  "href": "https://mylamp.example.com/oh",
                  "subprotocol": "longpoll"
                }
              ]
            }
          },
          "links": [
            {
              "href": "https://myswitch.example.com/"
            }
          ],
          "forms": [
            {
              "href": "https://mylamp.example.com/enumerate",
              "op": "readallproperties"
            }
          ],
          "securityDefinitions": {
            "nosec": {
              "scheme": "nosec"
            }
          },
          "security": [
            "nosec"
          ]
        }"#;

        let expected_thing = Thing {
            context: TD_CONTEXT.into(),
            id: Some("urn:dev:ops:32473-WoTLamp-1234".to_string()),
            attype: Some(vec!["Thing".to_string(), "LampThing".to_string()]),
            title: "MyLampThing".to_string(),
            titles: Some(
                [
                    ("en".to_string(), "MyLampThing".to_string()),
                    ("it".to_string(), "La mia lampada intelligente".to_string()),
                ]
                .into_iter()
                .collect(),
            ),
            description: Some("A simple smart lamp".to_string()),
            descriptions: Some(
                [
                    ("en".to_string(), "A simple smart lamp".to_string()),
                    (
                        "it".to_string(),
                        "Una semplice lampada intelligente".to_string(),
                    ),
                ]
                .into_iter()
                .collect(),
            ),
            version: Some(VersionInfo {
                instance: "0.1.0".to_string(),
            }),
            created: Some(datetime!(2022-05-01 10:20:42.123 UTC)),
            modified: Some(datetime!(2022-05-10 12:30 +1)),
            support: Some("mailto:mail@test.com".to_string()),
            base: Some("https://mylamp.example.com/".to_string()),
            properties: Some(
                [(
                    "status".to_string(),
                    PropertyAffordance {
                        interaction: InteractionAffordance {
                            forms: vec![Form {
                                op: DefaultedFormOperations::Default,
                                href: "https://mylamp.example.com/status".to_string(),
                                content_type: Form::default_content_type(),
                                content_coding: None,
                                subprotocol: None,
                                security: None,
                                scopes: None,
                                response: None,
                            }],
                            ..Default::default()
                        },
                        data_schema: DataSchema {
                            attype: None,
                            title: None,
                            titles: None,
                            description: None,
                            descriptions: None,
                            constant: None,
                            unit: None,
                            one_of: None,
                            enumeration: None,
                            read_only: false,
                            write_only: false,
                            format: None,
                            subtype: DataSchemaSubtype::String,
                        },
                        observable: None,
                    },
                )]
                .into_iter()
                .collect(),
            ),
            actions: Some(
                [(
                    "toggle".to_string(),
                    ActionAffordance {
                        interaction: InteractionAffordance {
                            forms: vec![Form {
                                op: DefaultedFormOperations::Default,
                                href: "https://mylamp.example.com/toggle".to_string(),
                                content_type: Form::default_content_type(),
                                content_coding: None,
                                subprotocol: None,
                                security: None,
                                scopes: None,
                                response: None,
                            }],
                            ..Default::default()
                        },
                        input: None,
                        output: None,
                        safe: false,
                        idempotent: false,
                    },
                )]
                .into_iter()
                .collect(),
            ),
            events: Some(
                [(
                    "overheating".to_string(),
                    EventAffordance {
                        interaction: InteractionAffordance {
                            forms: vec![Form {
                                op: DefaultedFormOperations::Default,
                                href: "https://mylamp.example.com/oh".to_string(),
                                content_type: Form::default_content_type(),
                                content_coding: None,
                                subprotocol: Some("longpoll".to_string()),
                                security: None,
                                scopes: None,
                                response: None,
                            }],
                            ..Default::default()
                        },
                        subscription: None,
                        data: Some(DataSchema {
                            attype: None,
                            title: None,
                            titles: None,
                            description: None,
                            descriptions: None,
                            constant: None,
                            unit: None,
                            one_of: None,
                            enumeration: None,
                            read_only: false,
                            write_only: false,
                            format: None,
                            subtype: DataSchemaSubtype::String,
                        }),
                        cancellation: None,
                    },
                )]
                .into_iter()
                .collect(),
            ),
            links: Some(vec![Link {
                href: "https://myswitch.example.com/".to_string(),
                ty: None,
                rel: None,
                anchor: None,
            }]),
            forms: Some(vec![Form {
                op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                href: "https://mylamp.example.com/enumerate".to_string(),
                content_type: Form::default_content_type(),
                content_coding: None,
                subprotocol: None,
                security: None,
                scopes: None,
                response: None,
            }]),
            security_definitions: [(
                "nosec".to_string(),
                SecurityScheme {
                    attype: None,
                    description: None,
                    descriptions: None,
                    proxy: None,
                    subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::NoSec),
                },
            )]
            .into_iter()
            .collect(),
            security: vec!["nosec".to_string()],
        };

        let thing: Thing = serde_json::from_str(RAW).unwrap();
        assert_eq!(thing, expected_thing);

        let thing: Thing = serde_json::from_value(serde_json::to_value(thing).unwrap()).unwrap();
        assert_eq!(thing, expected_thing);
    }

    #[test]
    fn default_context() {
        const RAW: &str = r#"
        {
          "title": "MyLampThing",
          "securityDefinitions": {
            "nosec": {
              "scheme": "nosec"
            }
          },
          "security": [
            "nosec"
          ]
        }"#;

        let expected_thing = Thing {
            context: TD_CONTEXT.into(),
            id: None,
            title: "MyLampThing".to_string(),
            security_definitions: [(
                "nosec".to_string(),
                SecurityScheme {
                    attype: None,
                    description: None,
                    descriptions: None,
                    proxy: None,
                    subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::NoSec),
                },
            )]
            .into_iter()
            .collect(),
            security: vec!["nosec".to_string()],
            attype: None,
            titles: None,
            description: None,
            descriptions: None,
            version: None,
            created: None,
            modified: None,
            support: None,
            base: None,
            properties: None,
            actions: None,
            events: None,
            links: None,
            forms: None,
        };

        let thing: Thing = serde_json::from_str(RAW).unwrap();
        assert_eq!(thing, expected_thing);
    }
}
