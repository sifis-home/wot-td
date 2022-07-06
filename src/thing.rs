//! Thing Description data structures
//!
//! A Thing Description, or `TD`, stores the semantic metadata and the interface descriptions of
//! a physical or virtual entity, called `Thing`.
//!
//! Use [Thing::build] to create a new `Thing`, [serde_json] to serialize or deserialize it.
//!
//! [Interaction Affordance]: https://www.w3.org/TR/wot-thing-description/#interactionaffordance

use std::{borrow::Cow, collections::HashMap, fmt};

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;
use serde_with::{serde_as, skip_serializing_none, DeserializeAs, OneOrMany, Same};
use time::OffsetDateTime;

use crate::{builder::ThingBuilder, extend::ExtendableThing, hlist::Nil};

pub(crate) type MultiLanguage = HashMap<String, String>;
pub(crate) type DataSchemaMap<T> = HashMap<String, DataSchema<T>>;

pub const TD_CONTEXT_10: &str = "https://www.w3.org/2019/wot/td/v1";
pub const TD_CONTEXT_11: &str = "https://www.w3.org/2019/wot/td/v1.1";

/// An abstraction of a physical or a virtual entity
///
/// It contains metadata and a description of its interfaces.
#[serde_as]
#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Thing<Other: ExtendableThing = Nil> {
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
    pub properties: Option<HashMap<String, PropertyAffordance<Other>>>,

    /// Action-based [Interaction Affordances]
    pub actions: Option<HashMap<String, ActionAffordance<Other>>>,

    /// Event-based [Interaction Affordances]
    pub events: Option<HashMap<String, EventAffordance<Other>>>,

    /// Arbitrary resources that relate to the current Thing
    ///
    /// Its meaning depends on the @context and the semantic attributes attached.
    pub links: Option<Vec<Link>>,

    /// Bulk-operations over the Thing properties
    pub forms: Option<Vec<Form<Other>>>,

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

    pub uri_variables: Option<DataSchemaMap<Other>>,

    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub profile: Option<Vec<String>>,

    #[serde(flatten)]
    pub other: Other::Thing,
}

impl<Other> fmt::Debug for Thing<Other>
where
    Other: ExtendableThing,
    PropertyAffordance<Other>: fmt::Debug,
    ActionAffordance<Other>: fmt::Debug,
    EventAffordance<Other>: fmt::Debug,
    Form<Other>: fmt::Debug,
    DataSchema<Other>: fmt::Debug,
    Other::Thing: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Thing")
            .field("context", &self.context)
            .field("id", &self.id)
            .field("attype", &self.attype)
            .field("title", &self.title)
            .field("titles", &self.titles)
            .field("description", &self.description)
            .field("descriptions", &self.descriptions)
            .field("version", &self.version)
            .field("created", &self.created)
            .field("modified", &self.modified)
            .field("support", &self.support)
            .field("base", &self.base)
            .field("properties", &self.properties)
            .field("actions", &self.actions)
            .field("events", &self.events)
            .field("links", &self.links)
            .field("forms", &self.forms)
            .field("security", &self.security)
            .field("security_definitions", &self.security_definitions)
            .field("uri_variables", &self.uri_variables)
            .field("profile", &self.profile)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> PartialEq for Thing<Other>
where
    Other: ExtendableThing,
    Other::Thing: PartialEq,
    Form<Other>: PartialEq,
    PropertyAffordance<Other>: PartialEq,
    ActionAffordance<Other>: PartialEq,
    EventAffordance<Other>: PartialEq,
    DataSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.context == other.context
            && self.id == other.id
            && self.attype == other.attype
            && self.title == other.title
            && self.titles == other.titles
            && self.description == other.description
            && self.descriptions == other.descriptions
            && self.version == other.version
            && self.created == other.created
            && self.modified == other.modified
            && self.support == other.support
            && self.base == other.base
            && self.properties == other.properties
            && self.actions == other.actions
            && self.events == other.events
            && self.links == other.links
            && self.forms == other.forms
            && self.security == other.security
            && self.security_definitions == other.security_definitions
            && self.uri_variables == other.uri_variables
            && self.profile == other.profile
            && self.other == other.other
    }
}

fn default_context() -> Value {
    TD_CONTEXT_11.into()
}

impl Thing<Nil> {
    /// Shorthand for [ThingBuilder::new].
    #[inline]
    pub fn build(title: impl Into<String>) -> ThingBuilder {
        ThingBuilder::new(title)
    }
}

#[serde_as]
#[skip_serializing_none]
#[derive(Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InteractionAffordance<Other: ExtendableThing = Nil> {
    #[serde(rename = "@type", default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub title: Option<String>,

    pub titles: Option<MultiLanguage>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    pub forms: Vec<Form<Other>>,

    pub uri_variables: Option<DataSchemaMap<Other>>,
}

impl<Other> PartialEq for InteractionAffordance<Other>
where
    Other: ExtendableThing,
    Form<Other>: PartialEq,
    DataSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.attype == other.attype
            && self.title == other.title
            && self.titles == other.titles
            && self.description == other.description
            && self.descriptions == other.descriptions
            && self.forms == other.forms
            && self.uri_variables == other.uri_variables
    }
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
pub struct PropertyAffordance<Other: ExtendableThing = Nil> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<Other>,

    #[serde(flatten)]
    pub data_schema: DataSchema<Other>,

    pub observable: Option<bool>,
}

impl<Other> PartialEq for PropertyAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: PartialEq,
    DataSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.interaction == other.interaction
            && self.data_schema == other.data_schema
            && self.observable == other.observable
    }
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
pub struct ActionAffordance<Other: ExtendableThing = Nil> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<Other>,

    pub input: Option<DataSchema<Other>>,

    pub output: Option<DataSchema<Other>>,

    #[serde(default)]
    pub safe: bool,

    #[serde(default)]
    pub idempotent: bool,

    pub synchronous: Option<bool>,
}

impl<Other> fmt::Debug for ActionAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: fmt::Debug,
    DataSchema<Other>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ActionAffordance")
            .field("interaction", &self.interaction)
            .field("input", &self.input)
            .field("output", &self.output)
            .field("safe", &self.safe)
            .field("idempotent", &self.idempotent)
            .field("synchronous", &self.synchronous)
            .finish()
    }
}

impl<Other> PartialEq for ActionAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: PartialEq,
    DataSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.interaction == other.interaction
            && self.input == other.input
            && self.output == other.output
            && self.safe == other.safe
            && self.idempotent == other.idempotent
            && self.synchronous == other.synchronous
    }
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
pub struct EventAffordance<Other: ExtendableThing = Nil> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<Other>,

    pub subscription: Option<DataSchema<Other>>,

    pub data: Option<DataSchema<Other>>,

    pub data_response: Option<DataSchema<Other>>,

    pub cancellation: Option<DataSchema<Other>>,
}

impl<Other> PartialEq for EventAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: PartialEq,
    DataSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.interaction == other.interaction
            && self.subscription == other.subscription
            && self.data == other.data
            && self.data_response == other.data_response
            && self.cancellation == other.cancellation
    }
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
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DataSchema<Other: ExtendableThing = Nil> {
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

    pub one_of: Option<Vec<DataSchema<Other>>>,

    #[serde(rename = "enum")]
    pub enumeration: Option<Vec<Value>>,

    #[serde(default)]
    pub read_only: bool,

    #[serde(default)]
    pub write_only: bool,

    pub format: Option<String>,

    #[serde(flatten)]
    pub subtype: Option<DataSchemaSubtype<Other>>,

    #[serde(flatten)]
    pub other: Other::DataSchema,
}

impl<Other> fmt::Debug for DataSchema<Other>
where
    Other: ExtendableThing,
    Other::Thing: fmt::Debug,
    Other::InteractionAffordance: fmt::Debug,
    Other::PropertyAffordance: fmt::Debug,
    Other::ActionAffordance: fmt::Debug,
    Other::EventAffordance: fmt::Debug,
    Other::Form: fmt::Debug,
    Other::ExpectedResponse: fmt::Debug,
    Other::DataSchema: fmt::Debug,
    Other::ObjectSchema: fmt::Debug,
    Other::ArraySchema: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DataSchema")
            .field("attype", &self.attype)
            .field("title", &self.title)
            .field("titles", &self.titles)
            .field("description", &self.description)
            .field("descriptions", &self.descriptions)
            .field("constant", &self.constant)
            .field("unit", &self.unit)
            .field("one_of", &self.one_of)
            .field("enumeration", &self.enumeration)
            .field("read_only", &self.read_only)
            .field("write_only", &self.write_only)
            .field("format", &self.format)
            .field("subtype", &self.subtype)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> PartialEq for DataSchema<Other>
where
    Other: ExtendableThing,
    Other::DataSchema: PartialEq,
    DataSchemaSubtype<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.attype == other.attype
            && self.title == other.title
            && self.titles == other.titles
            && self.description == other.description
            && self.descriptions == other.descriptions
            && self.constant == other.constant
            && self.unit == other.unit
            && self.one_of == other.one_of
            && self.enumeration == other.enumeration
            && self.read_only == other.read_only
            && self.write_only == other.write_only
            && self.format == other.format
            && self.subtype == other.subtype
            && self.other == other.other
    }
}

#[derive(Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum DataSchemaSubtype<Other: ExtendableThing = Nil> {
    Array(ArraySchema<Other>),
    Boolean,
    Number(NumberSchema),
    Integer(IntegerSchema),
    Object(ObjectSchema<Other>),
    String(StringSchema),
    Null,
}

impl<Other: ExtendableThing> Default for DataSchemaSubtype<Other> {
    fn default() -> Self {
        Self::Null
    }
}

impl<Other> fmt::Debug for DataSchemaSubtype<Other>
where
    Other: ExtendableThing,
    ArraySchema<Other>: fmt::Debug,
    ObjectSchema<Other>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Array(arg0) => f.debug_tuple("Array").field(arg0).finish(),
            Self::Boolean => write!(f, "Boolean"),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::Object(arg0) => f.debug_tuple("Object").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Null => write!(f, "Null"),
        }
    }
}

impl<Other> PartialEq for DataSchemaSubtype<Other>
where
    Other: ExtendableThing,
    ArraySchema<Other>: PartialEq,
    ObjectSchema<Other>: PartialEq,
    ArraySchema<Other>: PartialEq,
    ObjectSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Object(l0), Self::Object(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[serde_as]
#[skip_serializing_none]
#[derive(Default, Deserialize, Serialize)]
#[serde(bound(
    deserialize = "DataSchema<Other>: Deserialize<'de>",
    serialize = "DataSchema<Other>: Serialize"
))]
pub struct ArraySchema<Other: ExtendableThing = Nil> {
    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub items: Option<Vec<DataSchema<Other>>>,

    pub min_items: Option<u32>,

    pub max_items: Option<u32>,
}

impl<Other> fmt::Debug for ArraySchema<Other>
where
    Other: ExtendableThing,
    DataSchema<Other>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ArraySchema")
            .field("items", &self.items)
            .field("min_items", &self.min_items)
            .field("max_items", &self.max_items)
            .finish()
    }
}

impl<Other> PartialEq for ArraySchema<Other>
where
    Other: ExtendableThing,
    DataSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
            && self.min_items == other.min_items
            && self.max_items == other.max_items
    }
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
#[derive(Deserialize, Serialize)]
pub struct ObjectSchema<Other: ExtendableThing = Nil> {
    pub properties: Option<DataSchemaMap<Other>>,

    pub required: Option<Vec<String>>,

    #[serde(flatten)]
    pub other: Other::ObjectSchema,
}

impl<Other> fmt::Debug for ObjectSchema<Other>
where
    Other: ExtendableThing,
    Other::ObjectSchema: fmt::Debug,
    DataSchema<Other>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ObjectSchema")
            .field("properties", &self.properties)
            .field("required", &self.required)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> PartialEq for ObjectSchema<Other>
where
    Other: ExtendableThing,
    Other::ObjectSchema: PartialEq,
    DataSchema<Other>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.properties == other.properties
            && self.required == other.required
            && self.other == other.other
    }
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
#[derive(Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Form<Other: ExtendableThing = Nil> {
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

    pub response: Option<ExpectedResponse<Other::ExpectedResponse>>,

    #[serde(flatten)]
    pub other: Other::Form,
}

impl<Other> Clone for Form<Other>
where
    Other: ExtendableThing,
    Other::ExpectedResponse: Clone,
    Other::Form: Clone,
{
    fn clone(&self) -> Self {
        Self {
            op: self.op.clone(),
            href: self.href.clone(),
            content_type: self.content_type.clone(),
            content_coding: self.content_coding.clone(),
            subprotocol: self.subprotocol.clone(),
            security: self.security.clone(),
            scopes: self.scopes.clone(),
            response: self.response.clone(),
            other: self.other.clone(),
        }
    }
}

impl Form {
    pub(crate) const fn default_content_type() -> Cow<'static, str> {
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
pub struct ExpectedResponse<Other = Nil> {
    pub content_type: String,

    #[serde(flatten)]
    pub other: Other,
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
