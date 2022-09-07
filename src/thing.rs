//! Thing Description data structures
//!
//! A Thing Description, or `TD`, stores the semantic metadata and the interface descriptions of
//! a physical or virtual entity, called `Thing`.
//!
//! Use [Thing::build] to create a new `Thing`, [serde_json] to serialize or deserialize it.
//!
//! [Interaction Affordance]: https://www.w3.org/TR/wot-thing-description/#interactionaffordance

use std::{
    borrow::Cow,
    cmp::{self, Ordering},
    collections::HashMap,
    fmt,
};

use oxilangtag::LanguageTag;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;
use serde_with::{serde_as, skip_serializing_none, DeserializeAs, OneOrMany, Same};
use time::OffsetDateTime;

use crate::{
    builder::{data_schema::UncheckedDataSchema, ThingBuilder, ToExtend},
    extend::ExtendableThing,
    hlist::Nil,
};

pub(crate) type MultiLanguage = HashMap<LanguageTag<String>, String>;
pub(crate) type DataSchemaMap<Other> = HashMap<
    String,
    DataSchema<
        <Other as ExtendableThing>::DataSchema,
        <Other as ExtendableThing>::ArraySchema,
        <Other as ExtendableThing>::ObjectSchema,
    >,
>;

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

    pub schema_definitions: Option<DataSchemaMap<Other>>,

    #[serde(flatten)]
    pub other: Other,
}

impl<Other> fmt::Debug for Thing<Other>
where
    Other: ExtendableThing + fmt::Debug,
    PropertyAffordance<Other>: fmt::Debug,
    ActionAffordance<Other>: fmt::Debug,
    EventAffordance<Other>: fmt::Debug,
    Form<Other>: fmt::Debug,
    DataSchemaFromOther<Other>: fmt::Debug,
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
            .field("schema_definitions", &self.schema_definitions)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> Default for Thing<Other>
where
    Other: ExtendableThing + Default,
{
    fn default() -> Self {
        Self {
            context: Default::default(),
            id: Default::default(),
            attype: Default::default(),
            title: Default::default(),
            titles: Default::default(),
            description: Default::default(),
            descriptions: Default::default(),
            version: Default::default(),
            created: Default::default(),
            modified: Default::default(),
            support: Default::default(),
            base: Default::default(),
            properties: Default::default(),
            actions: Default::default(),
            events: Default::default(),
            links: Default::default(),
            forms: Default::default(),
            security: Default::default(),
            security_definitions: Default::default(),
            uri_variables: Default::default(),
            profile: Default::default(),
            schema_definitions: Default::default(),
            other: Default::default(),
        }
    }
}

impl<Other> PartialEq for Thing<Other>
where
    Other: ExtendableThing + PartialEq,
    Form<Other>: PartialEq,
    PropertyAffordance<Other>: PartialEq,
    ActionAffordance<Other>: PartialEq,
    EventAffordance<Other>: PartialEq,
    DataSchemaFromOther<Other>: PartialEq,
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
            && self.schema_definitions == other.schema_definitions
            && self.other == other.other
    }
}

fn default_context() -> Value {
    TD_CONTEXT_11.into()
}

impl Thing<Nil> {
    /// Shorthand for [ThingBuilder::new].
    #[inline]
    pub fn build(title: impl Into<String>) -> ThingBuilder<Nil, ToExtend> {
        ThingBuilder::new(title)
    }
}

#[serde_as]
#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InteractionAffordance<Other: ExtendableThing> {
    #[serde(rename = "@type", default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub title: Option<String>,

    pub titles: Option<MultiLanguage>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    pub forms: Vec<Form<Other>>,

    pub uri_variables: Option<DataSchemaMap<Other>>,

    #[serde(flatten)]
    pub other: Other::InteractionAffordance,
}

impl<Other> fmt::Debug for InteractionAffordance<Other>
where
    Other: ExtendableThing,
    Form<Other>: fmt::Debug,
    DataSchemaFromOther<Other>: fmt::Debug,
    Other::InteractionAffordance: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("InteractionAffordance")
            .field("attype", &self.attype)
            .field("title", &self.title)
            .field("titles", &self.titles)
            .field("description", &self.description)
            .field("descriptions", &self.descriptions)
            .field("forms", &self.forms)
            .field("uri_variables", &self.uri_variables)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> Default for InteractionAffordance<Other>
where
    Other: ExtendableThing,
    Form<Other>: Default,
    DataSchemaFromOther<Other>: Default,
    Other::InteractionAffordance: Default,
{
    fn default() -> Self {
        Self {
            attype: Default::default(),
            title: Default::default(),
            titles: Default::default(),
            description: Default::default(),
            descriptions: Default::default(),
            forms: Default::default(),
            uri_variables: Default::default(),
            other: Default::default(),
        }
    }
}

impl<Other> PartialEq for InteractionAffordance<Other>
where
    Other: ExtendableThing,
    Form<Other>: PartialEq,
    DataSchemaFromOther<Other>: PartialEq,
    Other::InteractionAffordance: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.attype == other.attype
            && self.title == other.title
            && self.titles == other.titles
            && self.description == other.description
            && self.descriptions == other.descriptions
            && self.forms == other.forms
            && self.uri_variables == other.uri_variables
            && self.other == other.other
    }
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
pub struct PropertyAffordance<Other: ExtendableThing> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<Other>,

    #[serde(flatten)]
    pub data_schema: DataSchemaFromOther<Other>,

    pub observable: Option<bool>,

    #[serde(flatten)]
    pub other: Other::PropertyAffordance,
}

impl<Other> fmt::Debug for PropertyAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: fmt::Debug,
    DataSchemaFromOther<Other>: fmt::Debug,
    Other::PropertyAffordance: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PropertyAffordance")
            .field("interaction", &self.interaction)
            .field("data_schema", &self.data_schema)
            .field("observable", &self.observable)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> Default for PropertyAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: Default,
    DataSchemaFromOther<Other>: Default,
    Other::PropertyAffordance: Default,
{
    fn default() -> Self {
        Self {
            interaction: Default::default(),
            data_schema: Default::default(),
            observable: Default::default(),
            other: Default::default(),
        }
    }
}

impl<Other> PartialEq for PropertyAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: PartialEq,
    DataSchemaFromOther<Other>: PartialEq,
    Other::PropertyAffordance: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.interaction == other.interaction
            && self.data_schema == other.data_schema
            && self.observable == other.observable
            && self.other == other.other
    }
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
pub struct ActionAffordance<Other: ExtendableThing> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<Other>,

    pub input: Option<DataSchemaFromOther<Other>>,

    pub output: Option<DataSchemaFromOther<Other>>,

    #[serde(default)]
    pub safe: bool,

    #[serde(default)]
    pub idempotent: bool,

    pub synchronous: Option<bool>,

    #[serde(flatten)]
    pub other: Other::ActionAffordance,
}

impl<Other> fmt::Debug for ActionAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: fmt::Debug,
    DataSchemaFromOther<Other>: fmt::Debug,
    Other::ActionAffordance: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ActionAffordance")
            .field("interaction", &self.interaction)
            .field("input", &self.input)
            .field("output", &self.output)
            .field("safe", &self.safe)
            .field("idempotent", &self.idempotent)
            .field("synchronous", &self.synchronous)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> Default for ActionAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: Default,
    DataSchemaFromOther<Other>: Default,
    Other::ActionAffordance: Default,
{
    fn default() -> Self {
        Self {
            interaction: Default::default(),
            input: Default::default(),
            output: Default::default(),
            safe: Default::default(),
            idempotent: Default::default(),
            synchronous: Default::default(),
            other: Default::default(),
        }
    }
}

impl<Other> PartialEq for ActionAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: PartialEq,
    DataSchemaFromOther<Other>: PartialEq,
    Other::ActionAffordance: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.interaction == other.interaction
            && self.input == other.input
            && self.output == other.output
            && self.safe == other.safe
            && self.idempotent == other.idempotent
            && self.synchronous == other.synchronous
            && self.other == other.other
    }
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize)]
pub struct EventAffordance<Other: ExtendableThing> {
    #[serde(flatten)]
    pub interaction: InteractionAffordance<Other>,

    pub subscription: Option<DataSchemaFromOther<Other>>,

    pub data: Option<DataSchemaFromOther<Other>>,

    pub data_response: Option<DataSchemaFromOther<Other>>,

    pub cancellation: Option<DataSchemaFromOther<Other>>,

    #[serde(flatten)]
    pub other: Other::EventAffordance,
}

impl<Other> fmt::Debug for EventAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: fmt::Debug,
    DataSchemaFromOther<Other>: fmt::Debug,
    Other::EventAffordance: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EventAffordance")
            .field("interaction", &self.interaction)
            .field("subscription", &self.subscription)
            .field("data", &self.data)
            .field("data_response", &self.data_response)
            .field("cancellation", &self.cancellation)
            .field("other", &self.other)
            .finish()
    }
}

impl<Other> Default for EventAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: Default,
    DataSchemaFromOther<Other>: Default,
    Other::EventAffordance: Default,
{
    fn default() -> Self {
        Self {
            interaction: Default::default(),
            subscription: Default::default(),
            data: Default::default(),
            data_response: Default::default(),
            cancellation: Default::default(),
            other: Default::default(),
        }
    }
}

impl<Other> PartialEq for EventAffordance<Other>
where
    Other: ExtendableThing,
    InteractionAffordance<Other>: PartialEq,
    DataSchemaFromOther<Other>: PartialEq,
    Other::EventAffordance: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.interaction == other.interaction
            && self.subscription == other.subscription
            && self.data == other.data
            && self.data_response == other.data_response
            && self.cancellation == other.cancellation
            && self.other == other.other
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
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DataSchema<DS, AS, OS> {
    #[serde(rename = "@type", default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub attype: Option<Vec<String>>,

    pub title: Option<String>,

    pub titles: Option<MultiLanguage>,

    pub description: Option<String>,

    pub descriptions: Option<MultiLanguage>,

    #[serde(rename = "const")]
    pub constant: Option<Value>,

    pub default: Option<Value>,

    pub unit: Option<String>,

    pub one_of: Option<Vec<Self>>,

    #[serde(rename = "enum")]
    pub enumeration: Option<Vec<Value>>,

    #[serde(default)]
    pub read_only: bool,

    #[serde(default)]
    pub write_only: bool,

    pub format: Option<String>,

    #[serde(flatten)]
    pub subtype: Option<DataSchemaSubtype<DS, AS, OS>>,

    #[serde(flatten)]
    pub other: DS,
}

pub(crate) type DataSchemaFromOther<Other> = DataSchema<
    <Other as ExtendableThing>::DataSchema,
    <Other as ExtendableThing>::ArraySchema,
    <Other as ExtendableThing>::ObjectSchema,
>;

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum DataSchemaSubtype<DS, AS, OS> {
    Array(ArraySchema<DS, AS, OS>),
    Boolean,
    Number(NumberSchema),
    Integer(IntegerSchema),
    Object(ObjectSchema<DS, AS, OS>),
    String(StringSchema),
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum UncheckedDataSchemaSubtype<DS, AS, OS> {
    Array(UncheckedArraySchema<DS, AS, OS>),
    Boolean,
    Number(NumberSchema),
    Integer(IntegerSchema),
    Object(UncheckedObjectSchema<DS, AS, OS>),
    String(StringSchema),
    Null,
}

impl<DS, AS, OS> Default for DataSchemaSubtype<DS, AS, OS> {
    fn default() -> Self {
        Self::Null
    }
}

#[serde_as]
#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(bound(
    deserialize = "DS: Deserialize<'de>, AS: Deserialize<'de>, OS: Deserialize<'de>",
    serialize = "DS: Serialize, AS: Serialize, OS: Serialize"
))]
pub struct ArraySchema<DS, AS, OS> {
    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub items: Option<Vec<DataSchema<DS, AS, OS>>>,

    pub min_items: Option<u32>,

    pub max_items: Option<u32>,

    #[serde(flatten)]
    pub other: AS,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct UncheckedArraySchema<DS, AS, OS> {
    pub(crate) items: Option<Vec<UncheckedDataSchema<DS, AS, OS>>>,
    pub(crate) min_items: Option<u32>,
    pub(crate) max_items: Option<u32>,
    pub(crate) other: AS,
}

impl<DS, AS, OS> Default for ArraySchema<DS, AS, OS>
where
    AS: Default,
{
    fn default() -> Self {
        Self {
            items: Default::default(),
            min_items: Default::default(),
            max_items: Default::default(),
            other: Default::default(),
        }
    }
}

/// A helper enum to represent an inclusive or exclusive maximum value.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub enum Maximum<T> {
    /// An inclusive maximum value.
    #[serde(rename = "maximum")]
    Inclusive(T),

    /// An exclusive maximum value.
    #[serde(rename = "exclusiveMaximum")]
    Exclusive(T),
}

/// A helper enum to represent an inclusive or exclusive minimum value.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub enum Minimum<T> {
    /// An inclusive minimum value.
    #[serde(rename = "minimum")]
    Inclusive(T),

    /// An exclusive minimum value.
    #[serde(rename = "exclusiveMinimum")]
    Exclusive(T),
}

impl<T> PartialOrd for Minimum<T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Minimum::Inclusive(a), Minimum::Inclusive(b))
            | (Minimum::Exclusive(a), Minimum::Exclusive(b)) => a.partial_cmp(b),
            (Minimum::Inclusive(a), Minimum::Exclusive(b)) => {
                a.partial_cmp(b).and_then(|ord| match ord {
                    Ordering::Less | Ordering::Equal => Some(Ordering::Less),
                    Ordering::Greater => None,
                })
            }
            (Minimum::Exclusive(a), Minimum::Inclusive(b)) => {
                a.partial_cmp(b).and_then(|ord| match ord {
                    Ordering::Less => None,
                    Ordering::Equal | Ordering::Greater => Some(Ordering::Greater),
                })
            }
        }
    }
}

impl<T> PartialOrd for Maximum<T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Maximum::Inclusive(a), Maximum::Inclusive(b))
            | (Maximum::Exclusive(a), Maximum::Exclusive(b)) => a.partial_cmp(b),

            (Maximum::Inclusive(a), Maximum::Exclusive(b)) => {
                a.partial_cmp(b).and_then(|ord| match ord {
                    Ordering::Less => None,
                    Ordering::Equal | Ordering::Greater => Some(Ordering::Greater),
                })
            }

            (Maximum::Exclusive(a), Maximum::Inclusive(b)) => {
                a.partial_cmp(b).and_then(|ord| match ord {
                    Ordering::Less | Ordering::Equal => Some(Ordering::Less),
                    Ordering::Greater => None,
                })
            }
        }
    }
}

impl<T> PartialEq<Maximum<T>> for Minimum<T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Maximum<T>) -> bool {
        match (self, other) {
            (Minimum::Inclusive(a), Maximum::Inclusive(b))
            | (Minimum::Exclusive(a), Maximum::Exclusive(b)) => a == b,
            _ => false,
        }
    }
}

impl<T> PartialEq<Minimum<T>> for Maximum<T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Minimum<T>) -> bool {
        other == self
    }
}

impl<T> PartialOrd<Maximum<T>> for Minimum<T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Maximum<T>) -> Option<cmp::Ordering> {
        match (self, other) {
            (Minimum::Inclusive(a), Maximum::Inclusive(b))
            | (Minimum::Exclusive(a), Maximum::Exclusive(b)) => a.partial_cmp(b),

            (Minimum::Exclusive(a), Maximum::Inclusive(b))
            | (Minimum::Inclusive(a), Maximum::Exclusive(b)) => {
                a.partial_cmp(b).and_then(|ord| match ord {
                    Ordering::Less => None,
                    Ordering::Equal | Ordering::Greater => Some(Ordering::Greater),
                })
            }
        }
    }
}

impl<T> PartialOrd<Minimum<T>> for Maximum<T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Minimum<T>) -> Option<Ordering> {
        other.partial_cmp(self).map(Ordering::reverse)
    }
}

macro_rules! impl_minmax_float {
    (@ $ty:ident $float_type:ty) => {
        impl $ty<$float_type> {
            pub fn is_nan(&self) -> bool {
                match self {
                    Self::Inclusive(x) => x.is_nan(),
                    Self::Exclusive(x) => x.is_nan(),
                }
            }
        }
    };

    ($($float_type:ty),*) => {
        $(
            impl_minmax_float!(@ Minimum $float_type);
            impl_minmax_float!(@ Maximum $float_type);
        )*
    };
}

impl_minmax_float!(f32, f64);

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NumberSchema {
    #[serde(flatten)]
    pub maximum: Option<Maximum<f64>>,

    #[serde(flatten)]
    pub minimum: Option<Minimum<f64>>,

    pub multiple_of: Option<f64>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Deserialize, Serialize)]
// FIXME: we should probably use a Decimal type
pub struct IntegerSchema {
    #[serde(flatten)]
    pub maximum: Option<Maximum<usize>>,

    #[serde(flatten)]
    pub minimum: Option<Minimum<usize>>,
}

#[skip_serializing_none]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct ObjectSchema<DS, AS, OS> {
    pub properties: Option<HashMap<String, DataSchema<DS, AS, OS>>>,

    pub required: Option<Vec<String>>,

    #[serde(flatten)]
    pub other: OS,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct UncheckedObjectSchema<DS, AS, OS> {
    pub(crate) properties: Option<HashMap<String, UncheckedDataSchema<DS, AS, OS>>>,
    pub(crate) required: Option<Vec<String>>,
    pub(crate) other: OS,
}

impl<DS, AS, OS> Default for ObjectSchema<DS, AS, OS>
where
    OS: Default,
{
    fn default() -> Self {
        Self {
            properties: Default::default(),
            required: Default::default(),
            other: Default::default(),
        }
    }
}

impl<DS, AS, OS> Default for UncheckedObjectSchema<DS, AS, OS>
where
    OS: Default,
{
    fn default() -> Self {
        Self {
            properties: Default::default(),
            required: Default::default(),
            other: Default::default(),
        }
    }
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
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
    Auto,
    Combo(ComboSecurityScheme),
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

#[serde_as]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum ComboSecurityScheme {
    OneOf(#[serde_as(as = "OneOrMany<_>")] Vec<String>),
    AllOf(#[serde_as(as = "OneOrMany<_>")] Vec<String>),
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
    Uri,
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
#[derive(Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Form<Other: ExtendableThing> {
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

    #[serde(default)]
    #[serde_as(as = "Option<OneOrMany<_>>")]
    pub additional_responses: Option<Vec<AdditionalExpectedResponse>>,

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
            additional_responses: self.additional_responses.clone(),
            other: self.other.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum FormOperation {
    ReadProperty,
    WriteProperty,
    ObserveProperty,
    UnobserveProperty,
    InvokeAction,
    QueryAction,
    CancelAction,
    SubscribeEvent,
    UnsubscribeEvent,
    ReadAllProperties,
    WriteAllProperties,
    ReadMultipleProperties,
    WriteMultipleProperties,
    ObserveAllProperties,
    UnobserveAllProperties,
    SubscribeAllEvents,
    UnsubscribeAllEvents,
    QueryAllActions,
}

impl fmt::Display for FormOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::ReadProperty => "readproperty",
            Self::WriteProperty => "writeproperty",
            Self::ObserveProperty => "observeproperty",
            Self::UnobserveProperty => "unobserveproperty",
            Self::InvokeAction => "invokeaction",
            Self::QueryAction => "queryaction",
            Self::CancelAction => "cancelaction",
            Self::SubscribeEvent => "subscribeevent",
            Self::UnsubscribeEvent => "unsubscribeevent",
            Self::ReadAllProperties => "readallproperties",
            Self::WriteAllProperties => "writeallproperties",
            Self::ReadMultipleProperties => "readmultipleproperties",
            Self::WriteMultipleProperties => "writemultipleproperties",
            Self::ObserveAllProperties => "observeallproperties",
            Self::UnobserveAllProperties => "unobserveallproperties",
            Self::SubscribeAllEvents => "subscribeallevents",
            Self::UnsubscribeAllEvents => "unsubscribeallevents",
            Self::QueryAllActions => "queryallactions",
        };

        f.write_str(s)
    }
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
pub struct ExpectedResponse<Other> {
    pub content_type: String,

    #[serde(flatten)]
    pub other: Other,
}

#[skip_serializing_none]
#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AdditionalExpectedResponse {
    #[serde(default = "bool_true", skip_serializing_if = "is_true")]
    pub success: bool,

    pub content_type: Option<String>,

    pub schema: Option<String>,
}

const fn bool_true() -> bool {
    true
}

const fn is_true(b: &bool) -> bool {
    *b
}

#[cfg(test)]
mod test {
    use serde_json::json;
    use time::macros::datetime;

    use crate::hlist::Cons;

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
          "schemaDefinitions": {
              "schema": {
                  "type": "null"
              }
          },
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
          ],
          "uriVariables": {
            "uriVariable1": {
              "type": "string"
            },
            "uriVariable2": {
              "type": "number"
            }
          }
        }"#;

        let expected_thing = Thing {
            context: TD_CONTEXT_11.into(),
            id: Some("urn:dev:ops:32473-WoTLamp-1234".to_string()),
            attype: Some(vec!["Thing".to_string(), "LampThing".to_string()]),
            title: "MyLampThing".to_string(),
            titles: Some(
                [
                    ("en".parse().unwrap(), "MyLampThing".to_string()),
                    (
                        "it".parse().unwrap(),
                        "La mia lampada intelligente".to_string(),
                    ),
                ]
                .into_iter()
                .collect(),
            ),
            description: Some("A simple smart lamp".to_string()),
            descriptions: Some(
                [
                    ("en".parse().unwrap(), "A simple smart lamp".to_string()),
                    (
                        "it".parse().unwrap(),
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
            schema_definitions: Some(
                [(
                    "schema".to_string(),
                    DataSchema {
                        subtype: Some(DataSchemaSubtype::Null),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            security_definitions: [("nosec".to_string(), SecurityScheme::default())]
                .into_iter()
                .collect(),
            security: vec!["nosec".to_string()],
            profile: Some(vec!["profile1".to_string(), "profile2".to_string()]),
            uri_variables: Some(
                [
                    (
                        "uriVariable1".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::String(Default::default())),
                            ..Default::default()
                        },
                    ),
                    (
                        "uriVariable2".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::Number(Default::default())),
                            ..Default::default()
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            ),
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

    #[derive(Serialize, Deserialize)]
    struct A(i32);

    impl Default for A {
        fn default() -> Self {
            A(42)
        }
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ThingExtA {
        a: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct IntAffExtA {
        b: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ActionAffExtA {
        c: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct PropAffExtA {
        d: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct EventAffExtA {
        e: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct FormExtA {
        f: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct RespExtA {
        g: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct DataSchemaExtA {
        h: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ObjectSchemaExtA {
        i: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ArraySchemaExtA {
        j: A,
    }

    impl ExtendableThing for ThingExtA {
        type InteractionAffordance = IntAffExtA;
        type PropertyAffordance = PropAffExtA;
        type ActionAffordance = ActionAffExtA;
        type EventAffordance = EventAffExtA;
        type Form = FormExtA;
        type ExpectedResponse = RespExtA;
        type DataSchema = DataSchemaExtA;
        type ObjectSchema = ObjectSchemaExtA;
        type ArraySchema = ArraySchemaExtA;
    }

    #[test]
    fn extend_single_thing() {
        let thing = Thing::<ThingExtA> {
            context: "test".into(),
            properties: Some(
                [(
                    "prop".to_string(),
                    PropertyAffordance {
                        interaction: InteractionAffordance {
                            other: IntAffExtA { b: A(1) },
                            ..Default::default()
                        },
                        data_schema: DataSchema {
                            subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                                other: ArraySchemaExtA { j: A(2) },
                                ..Default::default()
                            })),
                            other: DataSchemaExtA { h: A(3) },
                            ..Default::default()
                        },
                        other: PropAffExtA { d: A(4) },
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            actions: Some(
                [(
                    "action".to_string(),
                    ActionAffordance {
                        interaction: InteractionAffordance {
                            other: IntAffExtA { b: A(5) },
                            ..Default::default()
                        },
                        input: Some(DataSchema {
                            subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                                other: ObjectSchemaExtA { i: A(6) },
                                ..Default::default()
                            })),
                            other: DataSchemaExtA { h: A(7) },
                            ..Default::default()
                        }),
                        output: Some(DataSchema::default()),
                        other: ActionAffExtA { c: A(8) },
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            events: Some(
                [(
                    "event".to_string(),
                    EventAffordance {
                        other: EventAffExtA { e: A(9) },
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            forms: Some(vec![Form {
                response: Some(ExpectedResponse {
                    other: RespExtA { g: A(10) },
                    ..Default::default()
                }),
                other: FormExtA { f: A(11) },
                ..Default::default()
            }]),
            schema_definitions: Some(
                [(
                    "schema".to_string(),
                    DataSchema {
                        subtype: Some(DataSchemaSubtype::Null),
                        other: DataSchemaExtA { h: A(12) },
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            other: ThingExtA { a: A(13) },
            ..Default::default()
        };

        let thing_json = serde_json::to_value(thing).unwrap();
        assert_eq!(
            thing_json,
            json![{
                "@context": "test",
                "title": "",
                "properties": {
                    "prop": {
                        "b": 1,
                        "j": 2,
                        "h": 3,
                        "d": 4,
                        "forms": [],
                        "type": "array",
                        "readOnly": false,
                        "writeOnly": false,
                    }
                },
                "actions": {
                    "action": {
                        "b": 5,
                        "input": {
                            "i": 6,
                            "h": 7,
                            "readOnly": false,
                            "writeOnly": false,
                            "type": "object",
                        },
                        "output": {
                            "h": 42,
                            "readOnly": false,
                            "writeOnly": false,
                        },
                        "forms": [],
                        "idempotent": false,
                        "safe": false,
                        "c": 8,
                    }
                },
                "events": {
                    "event": {
                        "b": 42,
                        "e": 9,
                        "forms": [],
                    }
                },
                "forms": [{
                    "href": "",
                    "op": null,
                    "response": {
                        "contentType": "",
                        "g": 10,
                    },
                    "f": 11,
                }],
                "schemaDefinitions": {
                    "schema": {
                        "type": "null",
                        "readOnly": false,
                        "writeOnly": false,
                        "h": 12,
                    }
                },
                "security": [],
                "securityDefinitions": {},
                "a": 13,
            }],
        );
    }

    #[test]
    fn extend_single_thing_with_hlist() {
        let thing = Thing::<Cons<ThingExtA, Nil>> {
            context: "test".into(),
            properties: Some(
                [(
                    "prop".to_string(),
                    PropertyAffordance {
                        interaction: InteractionAffordance {
                            other: Nil::cons(IntAffExtA { b: A(1) }),
                            ..Default::default()
                        },
                        data_schema: DataSchema {
                            subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                                other: Nil::cons(ArraySchemaExtA { j: A(2) }),
                                ..Default::default()
                            })),
                            other: Nil::cons(DataSchemaExtA { h: A(3) }),
                            ..Default::default()
                        },
                        other: Nil::cons(PropAffExtA { d: A(4) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            actions: Some(
                [(
                    "action".to_string(),
                    ActionAffordance {
                        interaction: InteractionAffordance {
                            other: Nil::cons(IntAffExtA { b: A(5) }),
                            ..Default::default()
                        },
                        input: Some(DataSchema {
                            subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                                other: Nil::cons(ObjectSchemaExtA { i: A(6) }),
                                ..Default::default()
                            })),
                            other: Nil::cons(DataSchemaExtA { h: A(7) }),
                            ..Default::default()
                        }),
                        output: Some(DataSchema::default()),
                        other: Nil::cons(ActionAffExtA { c: A(8) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            events: Some(
                [(
                    "event".to_string(),
                    EventAffordance {
                        other: Nil::cons(EventAffExtA { e: A(9) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            forms: Some(vec![Form {
                response: Some(ExpectedResponse {
                    other: Nil::cons(RespExtA { g: A(10) }),
                    ..Default::default()
                }),
                other: Nil::cons(FormExtA { f: A(11) }),
                ..Default::default()
            }]),
            other: Nil::cons(ThingExtA { a: A(12) }),
            ..Default::default()
        };

        let thing_json = serde_json::to_value(thing).unwrap();
        assert_eq!(
            thing_json,
            json!({
                "@context": "test",
                "title": "",
                "properties": {
                    "prop": {
                        "b": 1,
                        "j": 2,
                        "h": 3,
                        "d": 4,
                        "forms": [],
                        "type": "array",
                        "readOnly": false,
                        "writeOnly": false,
                    }
                },
                "actions": {
                    "action": {
                        "b": 5,
                        "input": {
                            "i": 6,
                            "h": 7,
                            "readOnly": false,
                            "writeOnly": false,
                            "type": "object",
                        },
                        "output": {
                            "h": 42,
                            "readOnly": false,
                            "writeOnly": false,
                        },
                        "forms": [],
                        "idempotent": false,
                        "safe": false,
                        "c": 8,
                    }
                },
                "events": {
                    "event": {
                        "b": 42,
                        "e": 9,
                        "forms": [],
                    }
                },
                "forms": [{
                    "href": "",
                    "op": null,
                    "response": {
                        "contentType": "",
                        "g": 10,
                    },
                    "f": 11,
                }],
                "security": [],
                "securityDefinitions": {},
                "a": 12,
            }),
        );
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ThingExtB {
        k: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct IntAffExtB {
        l: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ActionAffExtB {
        m: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct PropAffExtB {
        n: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct EventAffExtB {
        o: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct FormExtB {
        p: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct RespExtB {
        q: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct DataSchemaExtB {
        r: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ObjectSchemaExtB {
        s: A,
    }

    #[derive(Default, Serialize, Deserialize)]
    struct ArraySchemaExtB {
        t: A,
    }

    impl ExtendableThing for ThingExtB {
        type InteractionAffordance = IntAffExtB;
        type PropertyAffordance = PropAffExtB;
        type ActionAffordance = ActionAffExtB;
        type EventAffordance = EventAffExtB;
        type Form = FormExtB;
        type ExpectedResponse = RespExtB;
        type DataSchema = DataSchemaExtB;
        type ObjectSchema = ObjectSchemaExtB;
        type ArraySchema = ArraySchemaExtB;
    }

    #[test]
    fn extend_thing_with_two() {
        let thing = Thing::<Cons<ThingExtB, Cons<ThingExtA, Nil>>> {
            context: "test".into(),
            properties: Some(
                [(
                    "prop".to_string(),
                    PropertyAffordance {
                        interaction: InteractionAffordance {
                            other: Nil::cons(IntAffExtA { b: A(1) }).cons(IntAffExtB { l: A(2) }),
                            ..Default::default()
                        },
                        data_schema: DataSchema {
                            subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                                other: Nil::cons(ArraySchemaExtA { j: A(3) })
                                    .cons(ArraySchemaExtB { t: A(4) }),
                                ..Default::default()
                            })),
                            other: Nil::cons(DataSchemaExtA { h: A(5) })
                                .cons(DataSchemaExtB { r: A(6) }),
                            ..Default::default()
                        },
                        other: Nil::cons(PropAffExtA { d: A(7) }).cons(PropAffExtB { n: A(8) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            actions: Some(
                [(
                    "action".to_string(),
                    ActionAffordance {
                        interaction: InteractionAffordance {
                            other: Nil::cons(IntAffExtA { b: A(9) }).cons(IntAffExtB { l: A(10) }),
                            ..Default::default()
                        },
                        input: Some(DataSchema {
                            subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                                other: Nil::cons(ObjectSchemaExtA { i: A(11) })
                                    .cons(ObjectSchemaExtB { s: A(12) }),
                                ..Default::default()
                            })),
                            other: Nil::cons(DataSchemaExtA { h: A(13) })
                                .cons(DataSchemaExtB { r: A(14) }),
                            ..Default::default()
                        }),
                        output: Some(DataSchema::default()),
                        other: Nil::cons(ActionAffExtA { c: A(15) })
                            .cons(ActionAffExtB { m: A(16) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            events: Some(
                [(
                    "event".to_string(),
                    EventAffordance {
                        other: Nil::cons(EventAffExtA { e: A(17) }).cons(EventAffExtB { o: A(18) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            forms: Some(vec![Form {
                response: Some(ExpectedResponse {
                    other: Nil::cons(RespExtA { g: A(19) }).cons(RespExtB { q: A(20) }),
                    ..Default::default()
                }),
                other: Nil::cons(FormExtA { f: A(21) }).cons(FormExtB { p: A(22) }),
                ..Default::default()
            }]),
            other: Nil::cons(ThingExtA { a: A(23) }).cons(ThingExtB { k: A(24) }),
            ..Default::default()
        };

        let thing_json = serde_json::to_value(thing).unwrap();
        assert_eq!(
            thing_json,
            json!({
                "@context": "test",
                "title": "",
                "properties": {
                    "prop": {
                        "b": 1,
                        "l": 2,
                        "j": 3,
                        "t": 4,
                        "h": 5,
                        "r": 6,
                        "d": 7,
                        "n": 8,
                        "forms": [],
                        "type": "array",
                        "readOnly": false,
                        "writeOnly": false,
                    }
                },
                "actions": {
                    "action": {
                        "b": 9,
                        "l": 10,
                        "input": {
                            "i": 11,
                            "s": 12,
                            "h": 13,
                            "r": 14,
                            "readOnly": false,
                            "writeOnly": false,
                            "type": "object",
                        },
                        "output": {
                            "h": 42,
                            "r": 42,
                            "readOnly": false,
                            "writeOnly": false,
                        },
                        "forms": [],
                        "idempotent": false,
                        "safe": false,
                        "c": 15,
                        "m": 16,
                    }
                },
                "events": {
                    "event": {
                        "b": 42,
                        "l": 42,
                        "e": 17,
                        "o": 18,
                        "forms": [],
                    }
                },
                "forms": [{
                    "href": "",
                    "op": null,
                    "response": {
                        "contentType": "",
                        "g": 19,
                        "q": 20,
                    },
                    "f": 21,
                    "p": 22,
                }],
                "security": [],
                "securityDefinitions": {},
                "a": 23,
                "k": 24,
            }),
        );
    }

    #[test]
    fn dummy_http() {
        #[derive(Serialize, Deserialize, Default)]
        struct HttpThing {}

        #[derive(Deserialize, Serialize)]
        #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
        enum HttpMethod {
            Get,
            Put,
            Post,
            Delete,
            Patch,
        }

        #[derive(Deserialize, Serialize)]
        struct HttpMessageHeader {
            #[serde(rename = "htv:fieldName")]
            field_name: Option<String>,
            #[serde(rename = "htv:fieldValue")]
            field_value: Option<String>,
        }

        #[derive(Deserialize, Serialize, Default)]
        struct HttpResponse {
            #[serde(rename = "htv:headers")]
            headers: Vec<HttpMessageHeader>,
            #[serde(rename = "htv:statusCodeValue")]
            status_code_value: Option<usize>,
        }

        #[derive(Default, Deserialize, Serialize)]
        struct HttpForm {
            #[serde(rename = "htv:methodName")]
            method_name: Option<HttpMethod>,
        }

        impl ExtendableThing for HttpThing {
            type InteractionAffordance = ();
            type PropertyAffordance = ();
            type ActionAffordance = ();
            type EventAffordance = ();
            type Form = HttpForm;
            type ExpectedResponse = HttpResponse;
            type DataSchema = ();
            type ObjectSchema = ();
            type ArraySchema = ();
        }

        let thing = Thing::<Cons<ThingExtB, Cons<HttpThing, Cons<ThingExtA, Nil>>>> {
            context: "test".into(),
            properties: Some(
                [(
                    "prop".to_string(),
                    PropertyAffordance {
                        interaction: InteractionAffordance {
                            other: Nil::cons(IntAffExtA { b: A(1) })
                                .cons(())
                                .cons(IntAffExtB { l: A(2) }),
                            ..Default::default()
                        },
                        data_schema: DataSchema {
                            subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                                other: Nil::cons(ArraySchemaExtA { j: A(3) })
                                    .cons(())
                                    .cons(ArraySchemaExtB { t: A(4) }),
                                ..Default::default()
                            })),
                            other: Nil::cons(DataSchemaExtA { h: A(5) })
                                .cons(())
                                .cons(DataSchemaExtB { r: A(6) }),
                            ..Default::default()
                        },
                        other: Nil::cons(PropAffExtA { d: A(7) })
                            .cons(())
                            .cons(PropAffExtB { n: A(8) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            actions: Some(
                [(
                    "action".to_string(),
                    ActionAffordance {
                        interaction: InteractionAffordance {
                            forms: vec![Form {
                                other: Nil::cons(FormExtA::default())
                                    .cons(HttpForm {
                                        method_name: Some(HttpMethod::Put),
                                    })
                                    .cons(FormExtB::default()),
                                ..Default::default()
                            }],
                            other: Nil::cons(IntAffExtA { b: A(9) })
                                .cons(())
                                .cons(IntAffExtB { l: A(10) }),
                            ..Default::default()
                        },
                        input: Some(DataSchema {
                            subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                                other: Nil::cons(ObjectSchemaExtA { i: A(11) })
                                    .cons(())
                                    .cons(ObjectSchemaExtB { s: A(12) }),
                                ..Default::default()
                            })),
                            other: Nil::cons(DataSchemaExtA { h: A(13) })
                                .cons(())
                                .cons(DataSchemaExtB { r: A(14) }),
                            ..Default::default()
                        }),
                        output: Some(DataSchema::default()),
                        other: Nil::cons(ActionAffExtA { c: A(15) })
                            .cons(())
                            .cons(ActionAffExtB { m: A(16) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            events: Some(
                [(
                    "event".to_string(),
                    EventAffordance {
                        other: Nil::cons(EventAffExtA { e: A(17) })
                            .cons(())
                            .cons(EventAffExtB { o: A(18) }),
                        ..Default::default()
                    },
                )]
                .into_iter()
                .collect(),
            ),
            forms: Some(vec![Form {
                response: Some(ExpectedResponse {
                    other: Nil::cons(RespExtA { g: A(19) })
                        .cons(HttpResponse {
                            headers: vec![HttpMessageHeader {
                                field_name: Some("hello".to_string()),
                                field_value: Some("world".to_string()),
                            }],
                            status_code_value: Some(200),
                        })
                        .cons(RespExtB { q: A(20) }),
                    ..Default::default()
                }),
                other: Nil::cons(FormExtA { f: A(21) })
                    .cons(HttpForm {
                        method_name: Some(HttpMethod::Get),
                    })
                    .cons(FormExtB { p: A(22) }),
                ..Default::default()
            }]),
            other: Nil::cons(ThingExtA { a: A(23) })
                .cons(HttpThing {})
                .cons(ThingExtB { k: A(24) }),
            ..Default::default()
        };

        let thing_json = serde_json::to_value(thing).unwrap();
        assert_eq!(
            thing_json,
            json!({
                "@context": "test",
                "title": "",
                "properties": {
                    "prop": {
                        "b": 1,
                        "l": 2,
                        "j": 3,
                        "t": 4,
                        "h": 5,
                        "r": 6,
                        "d": 7,
                        "n": 8,
                        "forms": [],
                        "type": "array",
                        "readOnly": false,
                        "writeOnly": false,
                    }
                },
                "actions": {
                    "action": {
                        "b": 9,
                        "l": 10,
                        "input": {
                            "i": 11,
                            "s": 12,
                            "h": 13,
                            "r": 14,
                            "readOnly": false,
                            "writeOnly": false,
                            "type": "object",
                        },
                        "output": {
                            "h": 42,
                            "r": 42,
                            "readOnly": false,
                            "writeOnly": false,
                        },
                        "forms": [
                            {
                                "f": 42,
                                "href": "",
                                "htv:methodName": "PUT",
                                "op": null,
                                "p": 42,
                            }
                        ],
                        "idempotent": false,
                        "safe": false,
                        "c": 15,
                        "m": 16,
                    }
                },
                "events": {
                    "event": {
                        "b": 42,
                        "l": 42,
                        "e": 17,
                        "o": 18,
                        "forms": [],
                    }
                },
                "forms": [{
                    "href": "",
                    "op": null,
                    "response": {
                        "contentType": "",
                        "g": 19,
                        "q": 20,
                        "htv:headers": [{
                            "htv:fieldName": "hello",
                            "htv:fieldValue": "world",
                        }],
                        "htv:statusCodeValue": 200,
                    },
                    "f": 21,
                    "p": 22,
                    "htv:methodName": "GET",
                }],
                "security": [],
                "securityDefinitions": {},
                "a": 23,
                "k": 24,
            }),
        );
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct DataSchemaExt {}

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ArraySchemaExt {}

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ObjectSchemaExt {}

    #[test]
    fn default_array_schema() {
        ArraySchema::<DataSchemaExt, (), ObjectSchemaExt>::default();
    }

    #[test]
    fn default_object_schema() {
        ObjectSchema::<DataSchemaExt, ArraySchemaExt, ()>::default();
    }

    #[test]
    fn serde_empty_additional_expected_response() {
        let response: AdditionalExpectedResponse = serde_json::from_value(json!({})).unwrap();
        assert_eq!(
            response,
            AdditionalExpectedResponse {
                success: true,
                content_type: None,
                schema: None,
            },
        );

        assert_eq!(serde_json::to_value(response).unwrap(), json!({}));
    }

    #[test]
    fn serde_full_additional_expected_response() {
        let raw_data = json!({
            "success": false,
            "contentType": "application/json",
            "schema": "test",
        });

        let response: AdditionalExpectedResponse =
            serde_json::from_value(raw_data.clone()).unwrap();

        assert_eq!(
            response,
            AdditionalExpectedResponse {
                success: false,
                content_type: Some("application/json".to_string()),
                schema: Some("test".to_string()),
            },
        );

        assert_eq!(serde_json::to_value(response).unwrap(), raw_data);
    }

    #[test]
    fn combo_security_scheme() {
        let raw_data = json!({
            "oneOf": "simple",
        });
        let combo: ComboSecurityScheme = serde_json::from_value(raw_data.clone()).unwrap();
        assert_eq!(
            combo,
            ComboSecurityScheme::OneOf(vec!["simple".to_string()]),
        );
        assert_eq!(serde_json::to_value(combo).unwrap(), raw_data);

        let raw_data = json!({
            "oneOf": ["data1", "data2"],
        });
        let combo: ComboSecurityScheme = serde_json::from_value(raw_data.clone()).unwrap();
        assert_eq!(
            combo,
            ComboSecurityScheme::OneOf(vec!["data1".to_string(), "data2".to_string()]),
        );
        assert_eq!(serde_json::to_value(combo).unwrap(), raw_data);

        let raw_data = json!({
            "allOf": "simple",
        });
        let combo: ComboSecurityScheme = serde_json::from_value(raw_data.clone()).unwrap();
        assert_eq!(
            combo,
            ComboSecurityScheme::AllOf(vec!["simple".to_string()]),
        );
        assert_eq!(serde_json::to_value(combo).unwrap(), raw_data);

        let raw_data = json!({
            "allOf": ["data1", "data2"],
        });
        let combo: ComboSecurityScheme = serde_json::from_value(raw_data.clone()).unwrap();
        assert_eq!(
            combo,
            ComboSecurityScheme::AllOf(vec!["data1".to_string(), "data2".to_string()]),
        );
        assert_eq!(serde_json::to_value(combo).unwrap(), raw_data);
    }

    #[test]
    fn minimum_partial_ord_trivial() {
        assert_eq!(
            Minimum::Inclusive(5).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Equal),
        );
        assert_eq!(
            Minimum::Inclusive(5).partial_cmp(&Minimum::Inclusive(6)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Minimum::Inclusive(6).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Greater),
        );

        assert_eq!(
            Minimum::Exclusive(5).partial_cmp(&Minimum::Exclusive(5)),
            Some(Ordering::Equal),
        );
        assert_eq!(
            Minimum::Exclusive(5).partial_cmp(&Minimum::Exclusive(6)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Minimum::Exclusive(6).partial_cmp(&Minimum::Exclusive(5)),
            Some(Ordering::Greater),
        );
    }

    #[test]
    fn minimum_partial_ord_complex() {
        assert_eq!(
            Minimum::Inclusive(4).partial_cmp(&Minimum::Exclusive(5)),
            Some(Ordering::Less),
        );

        assert_eq!(
            Minimum::Inclusive(5).partial_cmp(&Minimum::Exclusive(5)),
            Some(Ordering::Less),
        );

        assert_eq!(
            Minimum::Inclusive(6).partial_cmp(&Minimum::Exclusive(5)),
            None,
        );

        assert_eq!(
            Minimum::Exclusive(4).partial_cmp(&Minimum::Inclusive(5)),
            None,
        );

        assert_eq!(
            Minimum::Exclusive(5).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Greater),
        );

        assert_eq!(
            Minimum::Exclusive(6).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Greater),
        );
    }

    #[test]
    fn maximum_partial_ord_trivial() {
        use std::cmp::Ordering;
        assert_eq!(
            Maximum::Inclusive(5).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Equal),
        );
        assert_eq!(
            Maximum::Inclusive(5).partial_cmp(&Maximum::Inclusive(6)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Maximum::Inclusive(6).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Greater),
        );

        assert_eq!(
            Maximum::Exclusive(5).partial_cmp(&Maximum::Exclusive(5)),
            Some(Ordering::Equal),
        );
        assert_eq!(
            Maximum::Exclusive(5).partial_cmp(&Maximum::Exclusive(6)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Maximum::Exclusive(6).partial_cmp(&Maximum::Exclusive(5)),
            Some(Ordering::Greater),
        );
    }

    #[test]
    fn maximum_partial_ord_complex() {
        assert_eq!(
            Maximum::Inclusive(4).partial_cmp(&Maximum::Exclusive(5)),
            None,
        );

        assert_eq!(
            Maximum::Inclusive(5).partial_cmp(&Maximum::Exclusive(5)),
            Some(Ordering::Greater),
        );

        assert_eq!(
            Maximum::Inclusive(6).partial_cmp(&Maximum::Exclusive(5)),
            Some(Ordering::Greater),
        );

        assert_eq!(
            Maximum::Exclusive(4).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Less)
        );

        assert_eq!(
            Maximum::Exclusive(5).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Less),
        );

        assert_eq!(
            Maximum::Exclusive(6).partial_cmp(&Maximum::Inclusive(5)),
            None
        );
    }

    #[test]
    fn minimum_maximum_mixed_partial_ord_trivial() {
        assert_eq!(
            Minimum::Inclusive(4).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Minimum::Inclusive(5).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Equal),
        );
        assert_eq!(
            Minimum::Inclusive(6).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Greater),
        );

        assert_eq!(
            Maximum::Inclusive(4).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Maximum::Inclusive(5).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Equal),
        );
        assert_eq!(
            Maximum::Inclusive(6).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Greater),
        );
    }

    #[test]
    fn minimum_maximum_mixed_partial_ord_complex() {
        assert_eq!(
            Minimum::Inclusive(4).partial_cmp(&Maximum::Exclusive(5)),
            None,
        );
        assert_eq!(
            Minimum::Inclusive(5).partial_cmp(&Maximum::Exclusive(5)),
            Some(Ordering::Greater)
        );
        assert_eq!(
            Minimum::Inclusive(6).partial_cmp(&Maximum::Exclusive(5)),
            Some(Ordering::Greater)
        );

        assert_eq!(
            Minimum::Exclusive(4).partial_cmp(&Maximum::Inclusive(5)),
            None,
        );
        assert_eq!(
            Minimum::Exclusive(5).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Greater),
        );
        assert_eq!(
            Minimum::Exclusive(6).partial_cmp(&Maximum::Inclusive(5)),
            Some(Ordering::Greater),
        );

        assert_eq!(
            Maximum::Inclusive(4).partial_cmp(&Minimum::Exclusive(5)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Maximum::Inclusive(5).partial_cmp(&Minimum::Exclusive(5)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Maximum::Inclusive(6).partial_cmp(&Minimum::Exclusive(5)),
            None,
        );

        assert_eq!(
            Maximum::Exclusive(4).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Maximum::Exclusive(5).partial_cmp(&Minimum::Inclusive(5)),
            Some(Ordering::Less),
        );
        assert_eq!(
            Maximum::Exclusive(6).partial_cmp(&Minimum::Inclusive(5)),
            None,
        );
    }

    #[test]
    fn serde_number_schema() {
        let data: NumberSchema = serde_json::from_value(json! {
            {
                "minimum": 0.5,
                "maximum": 1.,
                "multipleOf": 0.5,
            }
        })
        .unwrap();

        assert_eq!(
            data,
            NumberSchema {
                minimum: Some(Minimum::Inclusive(0.5)),
                maximum: Some(Maximum::Inclusive(1.)),
                multiple_of: Some(0.5),
            },
        );

        let data: NumberSchema = serde_json::from_value(json! {
            {
                "exclusiveMinimum": 0.5,
                "exclusiveMaximum": 1.,
                "multipleOf": 0.5,
            }
        })
        .unwrap();

        assert_eq!(
            data,
            NumberSchema {
                minimum: Some(Minimum::Exclusive(0.5)),
                maximum: Some(Maximum::Exclusive(1.)),
                multiple_of: Some(0.5),
            },
        );
    }

    #[test]
    fn serde_integer_schema() {
        let data: IntegerSchema = serde_json::from_value(json! {
            {
                "minimum": 5,
                "maximum": 10,
            }
        })
        .unwrap();

        assert_eq!(
            data,
            IntegerSchema {
                minimum: Some(Minimum::Inclusive(5)),
                maximum: Some(Maximum::Inclusive(10)),
            },
        );

        let data: IntegerSchema = serde_json::from_value(json! {
            {
                "exclusiveMinimum": 5,
                "exclusiveMaximum": 10,
            }
        })
        .unwrap();

        assert_eq!(
            data,
            IntegerSchema {
                minimum: Some(Minimum::Exclusive(5)),
                maximum: Some(Maximum::Exclusive(10)),
            },
        );
    }
}
