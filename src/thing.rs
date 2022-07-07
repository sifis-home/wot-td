//! Thing Description data structures
//!
//! A Thing Description, or `TD`, stores the semantic metadata and the interface descriptions of
//! a physical or virtual entity, called `Thing`.
//!
//! Use [Thing::build] to create a new `Thing`, [serde_json] to serialize or deserialize it.
//!
//! [Interaction Affordance]: https://www.w3.org/TR/wot-thing-description/#interactionaffordance

use std::{borrow::Cow, collections::HashMap};

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;
use serde_with::{serde_as, skip_serializing_none, DeserializeAs, OneOrMany, Same};
use time::OffsetDateTime;

// use crate::builder::ThingBuilder;

use crate::hlist::Nil;
use crate::traits::{Buildable, Builder};

pub(crate) type MultiLanguage = HashMap<String, String>;
pub(crate) type DataSchemaMap = HashMap<String, DataSchema>;

pub const TD_CONTEXT_10: &str = "https://www.w3.org/2019/wot/td/v1";
pub const TD_CONTEXT_11: &str = "https://www.w3.org/2019/wot/td/v1.1";

fn default_context() -> Value {
    TD_CONTEXT_11.into()
}

impl Builder for Nil {
    type B = Nil;

    fn build(&self) -> Nil {
        Nil
    }
}

impl Buildable for Nil {
    type B = Nil;

    fn builder() -> Nil {
        Nil
    }
}

/// An abstraction of a physical or a virtual entity
///
/// It contains metadata and a description of its interfaces.
#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Thing<T: Buildable = Nil, I: Buildable = Nil> {
    // The context can be arbitrarily complex
    // https://www.w3.org/TR/json-ld11/#the-context
    // Let's take a value for now and assume we'll use the json-ld crate later
    /// A [JSON-LD @context](https://www.w3.org/TR/json-ld11/#the-context)
    #[serde(rename = "@context", default = "default_context")]
    pub context: Value,

    /// A unique identifier
    pub id: Option<String>,

    /// JSON-LD semantic keywords
    #[serde(rename = "@type", default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    /// Human-readable title to be displayed
    pub title: String,

    /// Multi-language translations of the title
    pub titles: Option<MultiLanguage>,

    /// Human-readable additional information
    pub description: Option<String>,

    /// Multi-language translations of the description
    pub descriptions: Option<MultiLanguage>,

    /// Version information
    pub version: Option<VersionInfo>,

    /// Time of creation of this description
    ///
    /// It may be used for caching purposes.
    #[serde(with = "time::serde::rfc3339::option", default)]
    pub created: Option<OffsetDateTime>,

    /// Time of last update of this description
    ///
    /// It may be used for caching purposes.
    #[serde(with = "time::serde::rfc3339::option", default)]
    pub modified: Option<OffsetDateTime>,

    /// URI to the device maintainer
    ///
    /// To be used to ask for support.
    // FIXME: use AnyURI
    pub support: Option<String>,

    /// Base URI to be used to resolve all the other relative URIs
    ///
    /// NOTE: the JSON-LD @context is excluded.
    // FIXME: use AnyURI
    pub base: Option<String>,

    /// Property-based [Interaction Affordances]
    pub properties: Option<HashMap<String, PropertyAffordance<I>>>,

    /// Action-based [Interaction Affordances]
    pub actions: Option<HashMap<String, ActionAffordance<I>>>,

    /// Event-based [Interaction Affordances]
    pub events: Option<HashMap<String, EventAffordance<I>>>,

    /// Arbitrary resources that relate to the current Thing
    ///
    /// Its meaning depends on the @context and the semantic attributes attached.
    pub links: Option<Vec<Link>>,

    /// Bulk-operations over the Thing properties
    pub forms: Option<Vec<Form>>,

    /// Thing-wide Security constraints
    ///
    /// It is a list of names matching the Security Schemes defined in [Thing::security_definitions].
    /// They must be all satisfied in order to access the Thing resources.
    #[serde_as(as = "OneOrMany<_>")]
    pub security: Vec<String>,

    /// Security definitions
    ///
    /// A Map of Security Schemes, the name keys are used in [Form::security] and [Thing::security]
    /// to express all the security constraints that must be satisfied in order to access the
    /// resources.
    pub security_definitions: HashMap<String, SecurityScheme>,

    pub uri_variables: Option<DataSchemaMap>,

    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub profile: Option<Vec<String>>,

    #[serde(flatten)]
    pub other: T,
}

/*
impl Thing {
    /// Shorthand for [ThingBuilder::new].
    #[inline]
    pub fn build(title: impl Into<String>) -> ThingBuilder {
        ThingBuilder::new(title)
    }
}
*/

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InteractionAffordance<I: Buildable> {
    #[serde(rename = "@type", default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub title: Option<String>,

    pub titles: Option<MultiLanguage>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    pub forms: Vec<Form>,

    pub uri_variables: Option<DataSchemaMap>,

    #[serde(flatten)]
    pub other: I,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct PropertyAffordance<I: Buildable> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<I>,

    #[serde(flatten)]
    pub data_schema: DataSchema,

    pub observable: Option<bool>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct ActionAffordance<I: Buildable> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<I>,

    pub input: Option<DataSchema>,

    pub output: Option<DataSchema>,

    #[serde(default)]
    pub safe: bool,

    #[serde(default)]
    pub idempotent: bool,

    pub synchronous: Option<bool>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct EventAffordance<I: Buildable> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<I>,

    pub subscription: Option<DataSchema>,

    pub data: Option<DataSchema>,

    pub data_response: Option<DataSchema>,

    pub cancellation: Option<DataSchema>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct VersionInfo {
    pub instance: String,

    pub model: Option<String>,
}

impl<S> From<S> for VersionInfo
where
    S: Into<String>,
{
    fn from(instance: S) -> Self {
        let instance = instance.into();
        Self {
            instance,
            model: None,
        }
    }
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DataSchema {
    #[serde(rename = "@type", default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

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
    pub subtype: Option<DataSchemaSubtype>,
}

#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum DataSchemaSubtype {
    Array(ArraySchema),
    Boolean,
    Number(NumberSchema),
    Integer(IntegerSchema),
    Object(ObjectSchema),
    String(StringSchema),
    #[default]
    Null,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct ArraySchema {
    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub items: Option<Vec<DataSchema>>,

    pub min_items: Option<u32>,

    pub max_items: Option<u32>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NumberSchema {
    pub maximum: Option<f64>,

    pub minimum: Option<f64>,

    pub multiple_of: Option<f64>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
// FIXME: we should probably use a Decimal type
pub struct IntegerSchema {
    pub maximum: Option<usize>,

    pub minimum: Option<usize>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct ObjectSchema {
    pub properties: Option<DataSchemaMap>,

    pub required: Option<Vec<String>>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct StringSchema {
    pub max_length: Option<u32>,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub struct SecurityScheme {
    #[serde(rename = "@type", default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    // FIXME: use AnyURI
    pub proxy: Option<String>,

    #[serde(flatten)]
    pub subtype: SecuritySchemeSubtype,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(tag = "scheme", rename_all = "lowercase")]
pub enum KnownSecuritySchemeSubtype {
    #[default]
    NoSec,
    Basic(BasicSecurityScheme),
    Digest(DigestSecurityScheme),
    Bearer(BearerSecurityScheme),
    Psk(PskSecurityScheme),
    OAuth2(OAuth2SecurityScheme),
    ApiKey(ApiKeySecurityScheme),
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
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

impl Default for SecuritySchemeSubtype {
    fn default() -> Self {
        Self::Known(KnownSecuritySchemeSubtype::default())
    }
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct BasicSecurityScheme {
    #[serde(rename = "in", default = "SecurityAuthenticationLocation::header")]
    pub location: SecurityAuthenticationLocation,
    pub name: Option<String>,
}

impl Default for BasicSecurityScheme {
    fn default() -> Self {
        Self {
            location: SecurityAuthenticationLocation::Header,
            name: Default::default(),
        }
    }
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

impl Default for DigestSecurityScheme {
    fn default() -> Self {
        Self {
            qop: Default::default(),
            location: SecurityAuthenticationLocation::Header,
            name: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum QualityOfProtection {
    #[default]
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

impl Default for ApiKeySecurityScheme {
    fn default() -> Self {
        Self {
            location: SecurityAuthenticationLocation::Query,
            name: Default::default(),
        }
    }
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

impl Default for BearerSecurityScheme {
    fn default() -> Self {
        Self {
            authorization: Default::default(),
            alg: BearerSecurityScheme::default_alg(),
            format: BearerSecurityScheme::default_format(),
            location: SecurityAuthenticationLocation::Header,
            name: Default::default(),
        }
    }
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
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct PskSecurityScheme {
    pub identity: Option<String>,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct OAuth2SecurityScheme {
    // FIXME: use AnyURI
    pub authorization: Option<String>,

    // FIXME: use AnyURI
    pub token: Option<String>,

    // FIXME: use AnyURI
    pub refresh: Option<String>,

    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub scopes: Option<Vec<String>>,

    pub flow: String,
}

impl OAuth2SecurityScheme {
    pub fn new(flow: impl Into<String>) -> Self {
        let flow = flow.into();
        Self {
            authorization: Default::default(),
            token: Default::default(),
            refresh: Default::default(),
            scopes: Default::default(),
            flow,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
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
#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Form {
    #[serde(default)]
    pub op: DefaultedFormOperations,

    // FIXME: use AnyURI
    pub href: String,

    pub content_type: Option<String>,

    // TODO: check if the subset of possible values is limited by the [IANA HTTP content coding
    // registry](https://www.iana.org/assignments/http-parameters/http-parameters.xhtml#content-coding).
    pub content_coding: Option<String>,

    pub subprotocol: Option<String>,

    // FIXME: use variant names of KnownSecuritySchemeSubtype + "other" string variant
    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub security: Option<Vec<String>>,

    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub scopes: Option<Vec<String>>,

    pub response: Option<ExpectedResponse>,
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

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum DefaultedFormOperations {
    #[default]
    Default,
    Custom(Vec<FormOperation>),
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

#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
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
            "@context": "https://www.w3.org/2019/wot/td/v1.1",
            "id": "urn:dev:ops:32473-WoTLamp-1234",
            "title": "MyLampThing",
            "securityDefinitions": {
                "nosec": {"scheme": "nosec"}
            },
            "security": ["nosec"]
        }"#;

        let expected_thing = Thing {
            context: TD_CONTEXT_11.into(),
            id: Some("urn:dev:ops:32473-WoTLamp-1234".to_string()),
            title: "MyLampThing".to_string(),
            security_definitions: [("nosec".to_string(), SecurityScheme::default())]
                .into_iter()
                .collect(),
            security: vec!["nosec".to_string()],
            ..Default::default()
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
          "@context": "https://www.w3.org/2019/wot/td/v1.1",
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
            "instance": "0.1.0",
            "model": "model"
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
              ],
              "synchronous": false
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
          ],
          "profile": [
              "profile1",
              "profile2"
          ]
        }"#;

        let expected_thing = Thing {
            context: TD_CONTEXT_11.into(),
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
                model: Some("model".to_string()),
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
                                href: "https://mylamp.example.com/status".to_string(),
                                ..Form::default()
                            }],
                            ..Default::default()
                        },
                        data_schema: DataSchema {
                            subtype: Some(DataSchemaSubtype::String(Default::default())),
                            ..Default::default()
                        },
                        ..Default::default()
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
                                href: "https://mylamp.example.com/toggle".to_string(),
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                        synchronous: Some(false),
                        ..Default::default()
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
                                href: "https://mylamp.example.com/oh".to_string(),
                                subprotocol: Some("longpoll".to_string()),
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                        data: Some(DataSchema {
                            subtype: Some(DataSchemaSubtype::String(StringSchema::default())),
                            ..Default::default()
                        }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            links: Some(vec![Link {
                href: "https://myswitch.example.com/".to_string(),
                ..Default::default()
            }]),
            forms: Some(vec![Form {
                op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                href: "https://mylamp.example.com/enumerate".to_string(),
                ..Default::default()
            }]),
            security_definitions: [("nosec".to_string(), SecurityScheme::default())]
                .into_iter()
                .collect(),
            security: vec!["nosec".to_string()],
            profile: Some(vec!["profile1".to_string(), "profile2".to_string()]),
            ..Default::default()
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
            context: TD_CONTEXT_11.into(),
            title: "MyLampThing".to_string(),
            security_definitions: [("nosec".to_string(), SecurityScheme::default())]
                .into_iter()
                .collect(),
            security: vec!["nosec".to_string()],
            ..Default::default()
        };

        let thing: Thing = serde_json::from_str(RAW).unwrap();
        assert_eq!(thing, expected_thing);
    }
}
