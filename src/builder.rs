pub mod affordance;
pub mod data_schema;
pub mod human_readable_info;

use std::{borrow::Cow, collections::HashMap, fmt, marker::PhantomData, ops::Not};

use serde_json::Value;
use time::OffsetDateTime;

use crate::{
    extend::{Extend, Extendable, ExtendableThing},
    thing::{
        ApiKeySecurityScheme, BasicSecurityScheme, BearerSecurityScheme, DataSchemaFromOther,
        DefaultedFormOperations, DigestSecurityScheme, ExpectedResponse, Form, FormOperation,
        KnownSecuritySchemeSubtype, Link, MultiLanguage, OAuth2SecurityScheme, PskSecurityScheme,
        QualityOfProtection, SecurityAuthenticationLocation, SecurityScheme, SecuritySchemeSubtype,
        Thing, UnknownSecuritySchemeSubtype, VersionInfo, TD_CONTEXT_11,
    },
};

use self::{
    affordance::{
        ActionAffordanceBuilder, AffordanceBuilder, CheckableInteractionAffordanceBuilder,
        EventAffordanceBuilder, IntoUsable, PropertyAffordanceBuilder,
        UsableActionAffordanceBuilder, UsableEventAffordanceBuilder,
        UsablePropertyAffordanceBuilder,
    },
    data_schema::{CheckableDataSchema, DataSchemaBuilder, PartialDataSchemaBuilder},
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ToExtend;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Extended;

/// Builder for WoT Thing
///
/// TODO: Write an example usage
#[must_use]
pub struct ThingBuilder<Other: ExtendableThing, Status> {
    context: Vec<Context>,
    id: Option<String>,
    attype: Option<Vec<String>>,
    title: String,
    titles: Option<MultiLanguage>,
    description: Option<String>,
    descriptions: Option<MultiLanguage>,
    version: Option<VersionInfo>,
    created: Option<OffsetDateTime>,
    modified: Option<OffsetDateTime>,
    support: Option<String>,
    base: Option<String>,
    properties: Vec<AffordanceBuilder<UsablePropertyAffordanceBuilder<Other>>>,
    actions: Vec<AffordanceBuilder<UsableActionAffordanceBuilder<Other>>>,
    events: Vec<AffordanceBuilder<UsableEventAffordanceBuilder<Other>>>,
    links: Option<Vec<Link>>,
    forms: Option<Vec<FormBuilder<Other, String, Other::Form>>>,
    uri_variables: Option<HashMap<String, DataSchemaFromOther<Other>>>,
    security: Vec<String>,
    security_definitions: Vec<(String, SecurityScheme)>,
    profile: Vec<String>,
    schema_definitions: HashMap<String, DataSchemaFromOther<Other>>,
    other: Other,
    _marker: PhantomData<Status>,
}

macro_rules! opt_field_builder {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            pub fn $field(mut self, value: impl Into<$ty>) -> Self {
                self.$field = Some(value.into());
                self
            }
        )*
    };
}

/// Builder errors
///
/// Most of the Thing Description conflicts are caught at compile time.
/// The few errors that may be discovered at only runtime are the following.
#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
pub enum Error {
    /// The WoT security definitions must have an unique name
    #[error("Two security definitions use the name \"{0}\"")]
    DuplicatedSecurityDefinition(String),

    /// The forms have defaults that depend on the Affordance that contains them.
    /// The Thing-level forms must be explicit on the operation
    #[error("A Form directly placed in a Thing must contain at least one relevant operation")]
    MissingOpInForm,

    /// The Thing-level forms must work on the Thing properties collectively at once.
    #[error("The operation of a Form directly placed in a Thing can only be one or more of the following: readallproperties, writeallproperties, readmultipleproperties, writemultipleproperties")]
    InvalidOpInForm,

    /// The security field must refer to existing security definitions.
    #[error("Security \"{0}\" is not specified in Thing security definitions")]
    UndefinedSecurity(String),

    /// When both min and max are specified, min must be less or equal than max
    #[error("Min value greater than max value")]
    InvalidMinMax,

    /// Neither minimum or maximum value can be NaN
    #[error("Min or Max value is NaN")]
    NanMinMax,

    /// For each type of affordance, names must be unique
    #[error("Two affordances of type {ty} use the name \"{name}\"")]
    DuplicatedAffordance {
        // The type of the affordance
        ty: AffordanceType,

        // The duplicated name
        name: String,
    },

    #[error("\"multipleOf\" field must be strictly greater than 0")]
    InvalidMultipleOf,
}

/// The possible affordance types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AffordanceType {
    /// A property affordance
    Property,

    /// An action affordance
    Action,

    /// An event affordance
    Event,
}

impl fmt::Display for AffordanceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Property => "property",
            Self::Action => "action",
            Self::Event => "event",
        };

        f.write_str(s)
    }
}

impl<Other: ExtendableThing> ThingBuilder<Other, ToExtend> {
    /// Create a new default builder with a specified title, using a default extension
    pub fn new(title: impl Into<String>) -> Self
    where
        Other: Default,
    {
        let title = title.into();
        let context = vec![Context::Simple(TD_CONTEXT_11.to_string())];

        Self {
            context,
            id: Default::default(),
            attype: Default::default(),
            title,
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
            _marker: PhantomData,
        }
    }

    /// Create a new default builder with a specified title, using an empty extension
    pub fn new_empty(title: impl Into<String>) -> ThingBuilder<Other::Empty, ToExtend>
    where
        Other: Extendable,
        Other::Empty: ExtendableThing,
    {
        let title = title.into();
        let context = vec![Context::Simple(TD_CONTEXT_11.to_string())];

        ThingBuilder {
            context,
            id: Default::default(),
            attype: Default::default(),
            title,
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
            other: Other::empty(),
            _marker: PhantomData,
        }
    }

    pub fn finish_extend(self) -> ThingBuilder<Other, Extended> {
        let Self {
            context,
            id,
            attype,
            title,
            titles,
            description,
            descriptions,
            version,
            created,
            modified,
            support,
            base,
            properties,
            actions,
            events,
            links,
            forms,
            uri_variables,
            security,
            security_definitions,
            profile,
            schema_definitions,
            other,
            _marker: _,
        } = self;

        ThingBuilder {
            context,
            id,
            attype,
            title,
            titles,
            description,
            descriptions,
            version,
            created,
            modified,
            support,
            base,
            properties,
            actions,
            events,
            links,
            forms,
            uri_variables,
            security,
            security_definitions,
            profile,
            schema_definitions,
            other,
            _marker: PhantomData,
        }
    }

    pub fn ext_with<F, T>(self, f: F) -> ThingBuilder<Other::Target, ToExtend>
    where
        F: FnOnce() -> T,
        Other: Extend<T>,
        Other::Target: ExtendableThing,
    {
        let Self {
            context,
            id,
            attype,
            title,
            titles,
            description,
            descriptions,
            version,
            created,
            modified,
            support,
            base,
            properties: _,
            actions: _,
            events: _,
            links,
            forms: _,
            uri_variables: _,
            security,
            security_definitions,
            profile,
            schema_definitions: _,
            other,
            _marker,
        } = self;

        let other = other.ext_with(f);
        ThingBuilder {
            context,
            id,
            attype,
            title,
            titles,
            description,
            descriptions,
            version,
            created,
            modified,
            support,
            base,
            properties: Default::default(),
            actions: Default::default(),
            events: Default::default(),
            links,
            forms: Default::default(),
            uri_variables: Default::default(),
            security,
            security_definitions,
            profile,
            schema_definitions: Default::default(),
            other,
            _marker,
        }
    }

    #[inline]
    pub fn ext<T>(self, t: T) -> ThingBuilder<Other::Target, ToExtend>
    where
        Other: Extend<T>,
        Other::Target: ExtendableThing,
    {
        self.ext_with(|| t)
    }
}

impl<Other: ExtendableThing, Status> ThingBuilder<Other, Status> {
    /// Consume the builder to produce the configured Thing
    ///
    /// This step will perform the final validation of the builder state.
    pub fn build(self) -> Result<Thing<Other>, Error> {
        use std::collections::hash_map::Entry;

        let Self {
            context,
            id,
            attype,
            title,
            titles,
            description,
            descriptions,
            version,
            created,
            modified,
            support,
            base,
            properties,
            actions,
            events,
            links,
            forms,
            security,
            security_definitions: security_definitions_vec,
            uri_variables,
            profile,
            schema_definitions,
            other,
            _marker: _,
        } = self;

        let mut security_definitions = HashMap::with_capacity(security_definitions_vec.len());
        for (name, scheme) in security_definitions_vec {
            match security_definitions.entry(name) {
                Entry::Vacant(entry) => {
                    entry.insert(scheme);
                }
                Entry::Occupied(entry) => {
                    return Err(Error::DuplicatedSecurityDefinition(entry.remove_entry().0));
                }
            }
        }

        let profile = profile.is_empty().not().then(|| profile);
        let schema_definitions = schema_definitions
            .is_empty()
            .not()
            .then(|| schema_definitions);

        let forms = forms
            .map(|forms| {
                forms
                    .into_iter()
                    .map(|form_builder| {
                        Self::build_form_from_builder(form_builder, &security_definitions)
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?;

        let context = {
            // TODO: improve this
            if context.len() == 1 {
                Value::String(context.into_iter().next().unwrap().into_simple().unwrap())
            } else {
                context
                    .into_iter()
                    .map(|context| match context {
                        Context::Simple(s) => Value::from(s),
                        Context::Map(map) => {
                            let map = map.into_iter().map(|(k, v)| (k, Value::from(v))).collect();
                            Value::Object(map)
                        }
                    })
                    .collect()
            }
        };

        let properties = try_build_affordance(
            properties,
            AffordanceType::Property,
            |property| &property.interaction,
            |property| [Some(&property.data_schema)],
            &security_definitions,
        )?;
        let actions = try_build_affordance(
            actions,
            AffordanceType::Action,
            |action| &action.interaction,
            |action| [action.input.as_ref(), action.output.as_ref()],
            &security_definitions,
        )?;
        let events = try_build_affordance(
            events,
            AffordanceType::Event,
            |event| &event.interaction,
            |event| {
                [
                    event.subscription.as_ref(),
                    event.data.as_ref(),
                    event.cancellation.as_ref(),
                ]
            },
            &security_definitions,
        )?;

        Ok(Thing {
            context,
            id,
            attype,
            title,
            titles,
            description,
            descriptions,
            version,
            created,
            modified,
            support,
            base,
            properties,
            actions,
            events,
            links,
            forms,
            security,
            security_definitions,
            uri_variables,
            profile,
            schema_definitions,
            other,
        })
    }

    fn build_form_from_builder(
        form_builder: FormBuilder<Other, String, Other::Form>,
        security_definitions: &HashMap<String, SecurityScheme>,
    ) -> Result<Form<Other>, Error> {
        use DefaultedFormOperations::*;
        use FormOperation::*;

        let FormBuilder {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            mut security,
            scopes,
            response,
            other,
            _marker: _,
        } = form_builder;

        security
            .as_mut()
            .map(|security| {
                security.iter_mut().try_for_each(|security| {
                    if security_definitions.contains_key(security) {
                        Ok(())
                    } else {
                        Err(Error::UndefinedSecurity(std::mem::take(security)))
                    }
                })
            })
            .transpose()?;

        match &op {
            Default => return Err(Error::MissingOpInForm),
            Custom(operations) => {
                let allowed_ops = operations.iter().all(|op| {
                    matches!(
                        op,
                        ReadAllProperties
                            | WriteAllProperties
                            | ReadMultipleProperties
                            | WriteMultipleProperties
                    )
                });

                if allowed_ops.not() {
                    return Err(Error::InvalidOpInForm);
                }
            }
        }

        Ok(Form {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            other,
        })
    }

    opt_field_builder!(
        id: String,
        description: String,
        version: VersionInfo,
        created: OffsetDateTime,
        modified: OffsetDateTime,
        support: String,
        base: String,
    );

    /// Add a new JSON-LD @context in the default namespace
    pub fn context<S>(mut self, value: S) -> Self
    where
        S: Into<String> + AsRef<str>,
    {
        if value.as_ref() == TD_CONTEXT_11 {
            return self;
        }

        let context = Context::Simple(value.into());
        self.context.push(context);
        self
    }

    /// Add a new JSON-LD @context with a custom namespace
    pub fn context_map<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut ContextMapBuilder) -> &mut ContextMapBuilder,
    {
        let mut context_map = ContextMapBuilder(Default::default());
        f(&mut context_map);

        self.context.push(Context::Map(context_map.0));
        self
    }

    /// Add a JSON-LD @type to the thing
    pub fn attype(mut self, value: impl Into<String>) -> Self {
        self.attype
            .get_or_insert_with(Default::default)
            .push(value.into());
        self
    }

    /// Set multi-language titles
    pub fn titles<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);

        self.titles = Some(builder.values);
        self
    }

    /// Set multi-language descriptions
    pub fn descriptions<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);
        self.descriptions = Some(builder.values);
        self
    }

    /// Add an additional link to the Thing Description
    pub fn link(mut self, href: impl Into<String>) -> Self {
        let href = href.into();

        let link = Link {
            href,
            ty: Default::default(),
            rel: Default::default(),
            anchor: Default::default(),
        };

        self.links.get_or_insert_with(Default::default).push(link);
        self
    }

    /// Add an additional link to the Thing Description, with specified optional fields.
    pub fn link_with<F>(mut self, f: F) -> Self
    where
        F: FnOnce(LinkBuilder<()>) -> LinkBuilder<String>,
    {
        let LinkBuilder {
            href,
            ty,
            rel,
            anchor,
        } = f(LinkBuilder::new());

        let link = Link {
            href,
            ty,
            rel,
            anchor,
        };

        self.links.get_or_insert_with(Default::default).push(link);
        self
    }

    /// Add a security definition and, eventually, a required security
    pub fn security<F, T>(mut self, f: F) -> Self
    where
        F: FnOnce(SecuritySchemeBuilder<()>) -> SecuritySchemeBuilder<T>,
        T: BuildableSecuritySchemeSubtype,
    {
        use SecuritySchemeSubtype::*;

        let builder = SecuritySchemeBuilder {
            attype: Default::default(),
            description: Default::default(),
            descriptions: Default::default(),
            proxy: Default::default(),
            name: Default::default(),
            subtype: Default::default(),
            required: false,
        };

        let SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype,
            required,
        } = f(builder);

        let subtype = subtype.build();
        let security_scheme = SecurityScheme {
            attype,
            description,
            descriptions,
            proxy,
            subtype,
        };

        let name = name.unwrap_or_else(|| {
            match &security_scheme.subtype {
                Known(KnownSecuritySchemeSubtype::NoSec) => "nosec",
                Known(KnownSecuritySchemeSubtype::Basic(_)) => "basic",
                Known(KnownSecuritySchemeSubtype::Digest(_)) => "digest",
                Known(KnownSecuritySchemeSubtype::Bearer(_)) => "bearer",
                Known(KnownSecuritySchemeSubtype::Psk(_)) => "psk",
                Known(KnownSecuritySchemeSubtype::OAuth2(_)) => "oauth2",
                Known(KnownSecuritySchemeSubtype::ApiKey(_)) => "apikey",
                Unknown(UnknownSecuritySchemeSubtype { scheme, .. }) => scheme.as_str(),
            }
            .to_string()
        });

        if required {
            self.security.push(name.clone());
        }
        self.security_definitions.push((name, security_scheme));

        self
    }

    pub fn profile(mut self, value: impl Into<String>) -> Self {
        self.profile.push(value.into());
        self
    }
}

impl<Other> ThingBuilder<Other, Extended>
where
    Other: ExtendableThing,
    Other::Form: Extendable,
{
    /// Add a Thing-level form
    ///
    /// NOTE:
    ///     - It must explicitly state its operation
    ///     - It must use an `all` operation
    pub fn form<F>(mut self, f: F) -> Self
    where
        F: FnOnce(
            FormBuilder<Other, (), <Other::Form as Extendable>::Empty>,
        ) -> FormBuilder<Other, String, Other::Form>,
    {
        self.forms
            .get_or_insert_with(Default::default)
            .push(f(FormBuilder::new()));
        self
    }
}

impl<Other> ThingBuilder<Other, Extended>
where
    Other: ExtendableThing,
{
    pub fn uri_variable<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(
            DataSchemaBuilder<
                <Other::DataSchema as Extendable>::Empty,
                Other::ArraySchema,
                Other::ObjectSchema,
                ToExtend,
            >,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        Other::DataSchema: Extendable,
    {
        self.uri_variables
            .get_or_insert_with(Default::default)
            .insert(
                name.into(),
                f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into(),
            );
        self
    }

    pub fn property<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(
            PropertyAffordanceBuilder<
                Other,
                PartialDataSchemaBuilder<
                    <Other::DataSchema as Extendable>::Empty,
                    Other::ArraySchema,
                    Other::ObjectSchema,
                    ToExtend,
                >,
                <Other::InteractionAffordance as Extendable>::Empty,
                <Other::PropertyAffordance as Extendable>::Empty,
            >,
        ) -> T,
        T: Into<
            PropertyAffordanceBuilder<
                Other,
                DataSchemaFromOther<Other>,
                Other::InteractionAffordance,
                Other::PropertyAffordance,
            >,
        >,
        Other::DataSchema: Extendable,
        Other::InteractionAffordance: Extendable,
        Other::PropertyAffordance: Extendable,
    {
        let affordance = f(PropertyAffordanceBuilder::empty()).into();
        let affordance_builder = AffordanceBuilder {
            name: name.into(),
            affordance,
        };
        self.properties.push(affordance_builder);
        self
    }

    pub fn action<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(
            ActionAffordanceBuilder<
                Other,
                <Other::InteractionAffordance as Extendable>::Empty,
                <Other::ActionAffordance as Extendable>::Empty,
            >,
        ) -> T,
        Other::InteractionAffordance: Extendable,
        Other::ActionAffordance: Extendable,
        T: IntoUsable<
            ActionAffordanceBuilder<Other, Other::InteractionAffordance, Other::ActionAffordance>,
        >,
    {
        let affordance = f(ActionAffordanceBuilder::empty()).into_usable();
        let affordance_builder = AffordanceBuilder {
            name: name.into(),
            affordance,
        };
        self.actions.push(affordance_builder);
        self
    }

    pub fn event<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(
            EventAffordanceBuilder<
                Other,
                <Other::InteractionAffordance as Extendable>::Empty,
                <Other::EventAffordance as Extendable>::Empty,
            >,
        ) -> T,
        Other::InteractionAffordance: Extendable,
        Other::EventAffordance: Extendable,
        T: IntoUsable<
            EventAffordanceBuilder<Other, Other::InteractionAffordance, Other::EventAffordance>,
        >,
    {
        let affordance = f(EventAffordanceBuilder::empty()).into_usable();
        let affordance_builder = AffordanceBuilder {
            name: name.into(),
            affordance,
        };
        self.events.push(affordance_builder);
        self
    }

    pub fn schema_definition<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(
            DataSchemaBuilder<
                <Other::DataSchema as Extendable>::Empty,
                Other::ArraySchema,
                Other::ObjectSchema,
                ToExtend,
            >,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        Other::DataSchema: Extendable,
    {
        self.schema_definitions.insert(
            name.into(),
            f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into(),
        );
        self
    }
}

fn try_build_affordance<A, F, IA, G, DS, T, const N: usize>(
    affordances: Vec<AffordanceBuilder<A>>,
    affordance_type: AffordanceType,
    mut get_interaction: F,
    mut get_data_schemas: G,
    security_definitions: &HashMap<String, SecurityScheme>,
) -> Result<Option<HashMap<String, T>>, Error>
where
    F: FnMut(&A) -> &IA,
    IA: CheckableInteractionAffordanceBuilder,
    G: FnMut(&A) -> [Option<&DS>; N],
    DS: CheckableDataSchema,
    A: Into<T>,
{
    use std::collections::hash_map::Entry;

    affordances
        .is_empty()
        .not()
        .then(|| {
            let new_affordances = HashMap::with_capacity(affordances.len());
            affordances
                .into_iter()
                .try_fold(new_affordances, |mut affordances, affordance| {
                    let AffordanceBuilder { name, affordance } = affordance;

                    get_interaction(&affordance).check(security_definitions)?;
                    get_data_schemas(&affordance)
                        .into_iter()
                        .flatten()
                        .try_for_each(CheckableDataSchema::check)?;

                    match affordances.entry(name) {
                        Entry::Vacant(entry) => {
                            entry.insert(affordance.into());
                            Ok(affordances)
                        }
                        Entry::Occupied(entry) => {
                            let name = entry.key().to_owned();
                            Err(Error::DuplicatedAffordance {
                                ty: affordance_type,
                                name,
                            })
                        }
                    }
                })
        })
        .transpose()
}

enum Context {
    Simple(String),
    Map(HashMap<String, String>),
}

impl Context {
    fn into_simple(self) -> Option<String> {
        match self {
            Self::Simple(s) => Some(s),
            _ => None,
        }
    }
}

/// Builder to create a structured JSON-LD @context with multiple namespaces
///
/// It is instantiated by [`ThingBuilder::context_map`]
#[must_use]
pub struct ContextMapBuilder(HashMap<String, String>);

impl ContextMapBuilder {
    /// Add a JSON-LD @context entry with a specific namespace
    pub fn context(&mut self, name: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.0.insert(name.into(), value.into());
        self
    }
}

/// Builder for language-specific variants of a field (e.g. titles, descriptions)
#[derive(Default)]
pub struct MultiLanguageBuilder<T> {
    values: HashMap<String, T>,
}

impl<T> MultiLanguageBuilder<T> {
    /// Add the language-specific variant
    ///
    /// NOTE: The language key is currently free-form
    pub fn add(&mut self, language: impl Into<String>, value: impl Into<T>) -> &mut Self {
        self.values.insert(language.into(), value.into());
        self
    }
}

/// Builder for Thing Description Links
pub struct LinkBuilder<Href> {
    href: Href,
    ty: Option<String>,
    rel: Option<String>,
    anchor: Option<String>,
}

impl LinkBuilder<()> {
    const fn new() -> Self {
        Self {
            href: (),
            ty: None,
            rel: None,
            anchor: None,
        }
    }

    /// Create a builder with the defined href
    pub fn href(self, value: impl Into<String>) -> LinkBuilder<String> {
        let Self {
            href: (),
            ty,
            rel,
            anchor,
        } = self;

        let href = value.into();
        LinkBuilder {
            href,
            ty,
            rel,
            anchor,
        }
    }
}

impl<T> LinkBuilder<T> {
    opt_field_builder!(ty: String, rel: String, anchor: String);
}

/// Builder for the Security Scheme
pub struct SecuritySchemeBuilder<S> {
    attype: Option<Vec<String>>,
    description: Option<String>,
    descriptions: Option<MultiLanguage>,
    proxy: Option<String>,
    name: Option<String>,
    subtype: S,
    required: bool,
}

/// Placeholder Type for the NoSecurity Scheme
pub struct SecuritySchemeNoSecTag;

/// Builder for the Security Scheme Subtype
pub trait BuildableSecuritySchemeSubtype {
    /// Consume the builder and produce the SecuritySchemeSubtype
    fn build(self) -> SecuritySchemeSubtype;
}

impl SecuritySchemeBuilder<()> {
    /// Default no-security scheme
    pub fn no_sec(self) -> SecuritySchemeBuilder<SecuritySchemeNoSecTag> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: SecuritySchemeNoSecTag,
            required,
        }
    }

    /// Basic Authentication RFC7617
    pub fn basic(self) -> SecuritySchemeBuilder<BasicSecurityScheme> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: BasicSecurityScheme::default(),
            required,
        }
    }

    /// Digest Assess Authentication RFC7616
    pub fn digest(self) -> SecuritySchemeBuilder<DigestSecurityScheme> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: DigestSecurityScheme::default(),
            required,
        }
    }

    /// Bearer Token RFC6750
    pub fn bearer(self) -> SecuritySchemeBuilder<BearerSecurityScheme> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: BearerSecurityScheme::default(),
            required,
        }
    }

    /// Pre-shared key authentication
    pub fn psk(self) -> SecuritySchemeBuilder<PskSecurityScheme> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: PskSecurityScheme::default(),
            required,
        }
    }

    /// OAuth2 authentication RFC6749 and RFC8252
    pub fn oauth2(self, flow: impl Into<String>) -> SecuritySchemeBuilder<OAuth2SecurityScheme> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: OAuth2SecurityScheme::new(flow),
            required,
        }
    }

    /// API key authentication
    pub fn apikey(self) -> SecuritySchemeBuilder<ApiKeySecurityScheme> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: ApiKeySecurityScheme::default(),
            required,
        }
    }

    /// Security scheme defined by an additional Vocabulary
    ///
    /// NOTE: Its definition MUST be in the Thing @context.
    pub fn custom(
        self,
        scheme: impl Into<String>,
    ) -> SecuritySchemeBuilder<UnknownSecuritySchemeSubtype> {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;

        let scheme = scheme.into();
        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: UnknownSecuritySchemeSubtype {
                scheme,
                data: Value::Null,
            },
            required,
        }
    }
}

impl<T> SecuritySchemeBuilder<T> {
    opt_field_builder!(description: String, proxy: String);

    /// JSON-LD @type
    pub fn attype(mut self, ty: impl Into<String>) -> Self {
        self.attype
            .get_or_insert_with(Default::default)
            .push(ty.into());
        self
    }

    /// Multi-language descriptions
    pub fn descriptions<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);
        self.descriptions = Some(builder.values);
        self
    }

    pub fn with_key(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn required(mut self) -> Self {
        self.required = true;
        self
    }
}

macro_rules! impl_buildable_known_security_scheme_subtype {
    ($($variant:ident => $ty:ty),* $(,)?) => {
        $(
            impl BuildableSecuritySchemeSubtype for $ty {
                fn build(self) -> SecuritySchemeSubtype {
                    SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::$variant(self))
                }
            }
        )*
    };
}

impl_buildable_known_security_scheme_subtype! (
    Basic => BasicSecurityScheme,
    Digest => DigestSecurityScheme,
    Bearer => BearerSecurityScheme,
    Psk => PskSecurityScheme,
    OAuth2 => OAuth2SecurityScheme,
    ApiKey => ApiKeySecurityScheme,
);

impl BuildableSecuritySchemeSubtype for SecuritySchemeNoSecTag {
    fn build(self) -> SecuritySchemeSubtype {
        SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::NoSec)
    }
}

impl BuildableSecuritySchemeSubtype for UnknownSecuritySchemeSubtype {
    fn build(self) -> SecuritySchemeSubtype {
        SecuritySchemeSubtype::Unknown(self)
    }
}

/// Accessor for the Name and Location fields
///
/// All the Security Schemes but NoSec have the `name` and location (`in`) fields.
pub trait HasNameLocation {
    /// Specifies the location of security authentication information
    fn location_mut(&mut self) -> &mut SecurityAuthenticationLocation;
    /// Name for query, header, or cookie parameters
    fn name_mut(&mut self) -> &mut Option<String>;
}

impl HasNameLocation for BasicSecurityScheme {
    fn location_mut(&mut self) -> &mut SecurityAuthenticationLocation {
        &mut self.location
    }

    fn name_mut(&mut self) -> &mut Option<String> {
        &mut self.name
    }
}

impl HasNameLocation for DigestSecurityScheme {
    fn location_mut(&mut self) -> &mut SecurityAuthenticationLocation {
        &mut self.location
    }

    fn name_mut(&mut self) -> &mut Option<String> {
        &mut self.name
    }
}

impl HasNameLocation for ApiKeySecurityScheme {
    fn location_mut(&mut self) -> &mut SecurityAuthenticationLocation {
        &mut self.location
    }

    fn name_mut(&mut self) -> &mut Option<String> {
        &mut self.name
    }
}

impl HasNameLocation for BearerSecurityScheme {
    fn location_mut(&mut self) -> &mut SecurityAuthenticationLocation {
        &mut self.location
    }

    fn name_mut(&mut self) -> &mut Option<String> {
        &mut self.name
    }
}

impl<T> SecuritySchemeBuilder<T>
where
    T: HasNameLocation,
{
    /// Name for query, header or cookie parameter
    pub fn name(mut self, value: impl Into<String>) -> Self {
        *self.subtype.name_mut() = Some(value.into());
        self
    }

    /// Location of the security authentication information
    pub fn location(mut self, value: SecurityAuthenticationLocation) -> Self {
        *self.subtype.location_mut() = value;
        self
    }
}

impl SecuritySchemeBuilder<DigestSecurityScheme> {
    /// Quality of protection
    pub fn qop(mut self, value: QualityOfProtection) -> Self {
        self.subtype.qop = value;
        self
    }
}

impl SecuritySchemeBuilder<BearerSecurityScheme> {
    /// URI of the authorization server
    pub fn authorization(mut self, value: impl Into<String>) -> Self {
        self.subtype.authorization = Some(value.into());
        self
    }

    /// Encoding, encryption or digest algorithm
    pub fn alg(mut self, value: impl Into<Cow<'static, str>>) -> Self {
        self.subtype.alg = value.into();
        self
    }

    /// Format of the security authentication information
    pub fn format(mut self, value: impl Into<Cow<'static, str>>) -> Self {
        self.subtype.format = value.into();
        self
    }
}

impl SecuritySchemeBuilder<OAuth2SecurityScheme> {
    /// URI of the authorization server
    pub fn authorization(mut self, value: impl Into<String>) -> Self {
        self.subtype.authorization = Some(value.into());
        self
    }

    /// URI of the token server
    pub fn token(mut self, value: impl Into<String>) -> Self {
        self.subtype.token = Some(value.into());
        self
    }

    /// URI of the refresh server
    pub fn refresh(mut self, value: impl Into<String>) -> Self {
        self.subtype.refresh = Some(value.into());
        self
    }

    /// Authorization scope identifier
    pub fn scope(mut self, value: impl Into<String>) -> Self {
        self.subtype
            .scopes
            .get_or_insert_with(Default::default)
            .push(value.into());
        self
    }
}

impl SecuritySchemeBuilder<UnknownSecuritySchemeSubtype> {
    /// JSON Value to be merged into the Scheme
    pub fn data(mut self, value: impl Into<Value>) -> Self {
        self.subtype.data = value.into();
        self
    }
}

/// Builder for the Form
pub struct FormBuilder<Other: ExtendableThing, Href, OtherForm> {
    op: DefaultedFormOperations,
    href: Href,
    content_type: Option<String>,
    content_coding: Option<String>,
    subprotocol: Option<String>,
    security: Option<Vec<String>>,
    scopes: Option<Vec<String>>,
    response: Option<ExpectedResponse<Other::ExpectedResponse>>,
    other: OtherForm,
    _marker: PhantomData<fn() -> Other>,
}

impl<Other> FormBuilder<Other, (), <Other::Form as Extendable>::Empty>
where
    Other: ExtendableThing,
    Other::Form: Extendable,
{
    fn new() -> Self {
        let other = <Other::Form as Extendable>::empty();

        Self {
            op: Default::default(),
            href: (),
            content_type: Default::default(),
            content_coding: Default::default(),
            subprotocol: Default::default(),
            security: Default::default(),
            scopes: Default::default(),
            response: Default::default(),
            other,
            _marker: PhantomData,
        }
    }
}

impl<Other, OtherForm> FormBuilder<Other, (), OtherForm>
where
    Other: ExtendableThing,
{
    /// Create a new builder with the specified Href
    pub fn href(self, value: impl Into<String>) -> FormBuilder<Other, String, OtherForm> {
        let Self {
            op,
            href: (),
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            other,
            _marker,
        } = self;

        let href = value.into();
        FormBuilder {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            other,
            _marker,
        }
    }
}

impl<Other, Href, OtherForm> FormBuilder<Other, Href, OtherForm>
where
    Other: ExtendableThing,
{
    opt_field_builder!(
        content_type: String,
        content_coding: String,
        subprotocol: String,
    );

    /// Set the form intended operation
    ///
    /// Depending on its parent the form may have a Default operation
    /// or it must be explicitly set.
    pub fn op(mut self, new_op: FormOperation) -> Self {
        match &mut self.op {
            ops @ DefaultedFormOperations::Default => {
                *ops = DefaultedFormOperations::Custom(vec![new_op])
            }
            DefaultedFormOperations::Custom(ops) => ops.push(new_op),
        }

        self
    }

    /// Set the security definitions that must be satisfied to access the resource
    ///
    /// They must be set beforehand by [Thing::security].
    pub fn security(mut self, value: impl Into<String>) -> Self {
        self.security
            .get_or_insert_with(Default::default)
            .push(value.into());
        self
    }

    /// Set the authorization scope identifiers
    ///
    /// It requires an OAuth2 Security Scheme
    pub fn scope(mut self, value: impl Into<String>) -> Self {
        self.scopes
            .get_or_insert_with(Default::default)
            .push(value.into());
        self
    }

    pub fn ext_with<F, T>(self, f: F) -> FormBuilder<Other, Href, OtherForm::Target>
    where
        OtherForm: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            other,
            _marker,
        } = self;
        let other = other.ext_with(f);
        FormBuilder {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            other,
            _marker,
        }
    }

    #[inline]
    pub fn ext<T>(self, t: T) -> FormBuilder<Other, Href, OtherForm::Target>
    where
        OtherForm: Extend<T>,
    {
        self.ext_with(move || t)
    }
}

impl<Other, T, OtherForm> FormBuilder<Other, T, OtherForm>
where
    Other: ExtendableThing,
    Other::ExpectedResponse: Default,
{
    /// Set the expected response metadata
    ///
    /// It is optional if the input and output metadata are the same, e.g. the content_type
    /// matches.
    pub fn response_default_ext(mut self, content_type: impl Into<String>) -> Self {
        self.response = Some(ExpectedResponse {
            content_type: content_type.into(),
            other: Default::default(),
        });
        self
    }
}

impl<Other, T, OtherForm> FormBuilder<Other, T, OtherForm>
where
    Other: ExtendableThing,
    Other::ExpectedResponse: Extendable,
{
    /// Set the expected response metadata building the type from ground up
    ///
    /// It is optional if the input and output metadata are the same, e.g. the content_type
    /// matches.
    pub fn response<F>(mut self, content_type: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(<Other::ExpectedResponse as Extendable>::Empty) -> Other::ExpectedResponse,
    {
        self.response = Some(ExpectedResponse {
            content_type: content_type.into(),
            other: f(Other::ExpectedResponse::empty()),
        });
        self
    }
}

impl<Other> From<FormBuilder<Other, String, Other::Form>> for Form<Other>
where
    Other: ExtendableThing,
{
    fn from(builder: FormBuilder<Other, String, Other::Form>) -> Self {
        let FormBuilder {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            other,
            _marker: _,
        } = builder;

        Self {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            other,
        }
    }
}

#[cfg(test)]
mod tests {
    use serde::{Deserialize, Serialize};
    use serde_json::json;
    use time::macros::datetime;

    use crate::{
        builder::{
            affordance::BuildableInteractionAffordance,
            data_schema::{NumberDataSchemaBuilderLike, SpecializableDataSchema},
            human_readable_info::BuildableHumanReadableInfo,
        },
        hlist::{Cons, Nil},
        thing::{
            ActionAffordance, ArraySchema, DataSchema, DataSchemaSubtype, EventAffordance,
            InteractionAffordance, NumberSchema, ObjectSchema, PropertyAffordance,
        },
    };

    use super::*;

    macro_rules! test_opt_string_field_builder {
        ($($field:ident),* $(,)?) => {
            $(
                #[test]
                pub fn $field() {
                    let thing = ThingBuilder::<Nil, _>::new("MyLampThing").finish_extend().$field("test").build().unwrap();

                    assert_eq!(
                        thing,
                        Thing {
                            context: TD_CONTEXT_11.into(),
                            title: "MyLampThing".to_string(),
                            $field: Some("test".into()),
                            ..Default::default()
                        }
                    );
                }
            )*
        };
    }

    #[test]
    fn default_context() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .build()
            .unwrap();
        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                ..Default::default()
            }
        )
    }

    #[test]
    fn redundant_default_context() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .context(TD_CONTEXT_11)
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                ..Default::default()
            }
        )
    }

    #[test]
    fn simple_contexts() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .context("test")
            .context("another_test")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: json! {[
                    TD_CONTEXT_11,
                    "test",
                    "another_test",
                ]},
                title: "MyLampThing".to_string(),
                ..Default::default()
            }
        )
    }

    #[test]
    fn map_contexts() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .context_map(|b| b.context("hello", "world").context("all", "fine"))
            .context("simple")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: json! {[
                    TD_CONTEXT_11,
                    {
                        "hello": "world",
                        "all": "fine",
                    },
                    "simple",
                ]},
                title: "MyLampThing".to_string(),
                ..Default::default()
            }
        )
    }

    test_opt_string_field_builder!(id, description, version, support, base);

    #[test]
    fn attype() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .attype("test")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                attype: Some(vec!["test".to_string()]),
                ..Default::default()
            }
        );

        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .attype("test1")
            .attype("test2")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                attype: Some(vec!["test1".to_string(), "test2".to_string()]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn titles() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .titles(|ml| ml.add("en", "My lamp").add("it", "La mia lampada"))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                titles: Some(
                    [("en", "My lamp"), ("it", "La mia lampada")]
                        .into_iter()
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .collect()
                ),
                ..Default::default()
            }
        );
    }

    #[test]
    fn descriptions() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .description("My Lamp")
            .descriptions(|ml| ml.add("en", "My lamp").add("it", "La mia lampada"))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                description: Some("My Lamp".to_string()),
                descriptions: Some(
                    [("en", "My lamp"), ("it", "La mia lampada")]
                        .into_iter()
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .collect()
                ),
                ..Default::default()
            }
        );
    }

    #[test]
    fn created() {
        const DATETIME: OffsetDateTime = datetime!(2022-05-01 12:13:14.567 +01:00);
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .created(DATETIME)
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                created: Some(DATETIME),
                ..Default::default()
            }
        );
    }

    #[test]
    fn modified() {
        const DATETIME: OffsetDateTime = datetime!(2022-05-01 12:13:14.567 +01:00);
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .modified(DATETIME)
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                modified: Some(DATETIME),
                ..Default::default()
            }
        );
    }

    #[test]
    fn link_simple() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .link("href1")
            .link("href2")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                links: Some(vec![
                    Link {
                        href: "href1".to_string(),
                        ty: Default::default(),
                        rel: Default::default(),
                        anchor: Default::default(),
                    },
                    Link {
                        href: "href2".to_string(),
                        ty: Default::default(),
                        rel: Default::default(),
                        anchor: Default::default(),
                    }
                ]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn link_with() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .link_with(|link| link.href("href1").ty("ty").rel("rel").anchor("anchor"))
            .link_with(|link| link.href("href2"))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                links: Some(vec![
                    Link {
                        href: "href1".to_string(),
                        ty: Some("ty".to_string()),
                        rel: Some("rel".to_string()),
                        anchor: Some("anchor".to_string()),
                    },
                    Link {
                        href: "href2".to_string(),
                        ty: Default::default(),
                        rel: Default::default(),
                        anchor: Default::default(),
                    }
                ]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn nosec_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.no_sec()
                    .attype("ty1")
                    .attype("ty2")
                    .description("desc")
                    .descriptions(|ml| ml.add("en", "desc_en").add("it", "desc_it"))
                    .proxy("proxy")
                    .required()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["nosec".to_string()],
                security_definitions: [(
                    "nosec".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".to_string(), "desc_en".to_string()),
                                ("it".to_string(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::NoSec)
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn basic_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.basic()
                    .name("name")
                    .location(SecurityAuthenticationLocation::Cookie)
                    .attype("ty1")
                    .attype("ty2")
                    .description("desc")
                    .descriptions(|ml| ml.add("en", "desc_en").add("it", "desc_it"))
                    .proxy("proxy")
                    .required()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["basic".to_string()],
                security_definitions: [(
                    "basic".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".to_string(), "desc_en".to_string()),
                                ("it".to_string(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Basic(
                            BasicSecurityScheme {
                                location: SecurityAuthenticationLocation::Cookie,
                                name: Some("name".to_string())
                            }
                        ))
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn digest_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.digest()
                    .name("name")
                    .location(SecurityAuthenticationLocation::Cookie)
                    .qop(QualityOfProtection::AuthInt)
                    .attype("ty1")
                    .attype("ty2")
                    .description("desc")
                    .descriptions(|ml| ml.add("en", "desc_en").add("it", "desc_it"))
                    .proxy("proxy")
                    .required()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["digest".to_string()],
                security_definitions: [(
                    "digest".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".to_string(), "desc_en".to_string()),
                                ("it".to_string(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Digest(
                            DigestSecurityScheme {
                                location: SecurityAuthenticationLocation::Cookie,
                                name: Some("name".to_string()),
                                qop: QualityOfProtection::AuthInt,
                            }
                        ))
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn apikey_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.apikey()
                    .name("name")
                    .location(SecurityAuthenticationLocation::Cookie)
                    .attype("ty1")
                    .attype("ty2")
                    .description("desc")
                    .descriptions(|ml| ml.add("en", "desc_en").add("it", "desc_it"))
                    .proxy("proxy")
                    .required()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["apikey".to_string()],
                security_definitions: [(
                    "apikey".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".to_string(), "desc_en".to_string()),
                                ("it".to_string(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::ApiKey(
                            ApiKeySecurityScheme {
                                location: SecurityAuthenticationLocation::Cookie,
                                name: Some("name".to_string()),
                            }
                        ))
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn bearer_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.bearer()
                    .name("name")
                    .location(SecurityAuthenticationLocation::Cookie)
                    .authorization("authorization")
                    .alg("alg")
                    .format("format".to_string())
                    .attype("ty1")
                    .attype("ty2")
                    .description("desc")
                    .descriptions(|ml| ml.add("en", "desc_en").add("it", "desc_it"))
                    .proxy("proxy")
                    .required()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["bearer".to_string()],
                security_definitions: [(
                    "bearer".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".to_string(), "desc_en".to_string()),
                                ("it".to_string(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Bearer(
                            BearerSecurityScheme {
                                location: SecurityAuthenticationLocation::Cookie,
                                name: Some("name".to_string()),
                                authorization: Some("authorization".to_string()),
                                alg: Cow::Borrowed("alg"),
                                format: Cow::Borrowed("format"),
                            }
                        ))
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn oauth2_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.oauth2("flow")
                    .authorization("authorization")
                    .token("token")
                    .refresh("refresh")
                    .scope("scope1")
                    .scope("scope2")
                    .attype("ty1")
                    .attype("ty2")
                    .description("desc")
                    .descriptions(|ml| ml.add("en", "desc_en").add("it", "desc_it"))
                    .proxy("proxy")
                    .required()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["oauth2".to_string()],
                security_definitions: [(
                    "oauth2".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".to_string(), "desc_en".to_string()),
                                ("it".to_string(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::OAuth2(
                            OAuth2SecurityScheme {
                                authorization: Some("authorization".to_string()),
                                token: Some("token".to_string()),
                                refresh: Some("refresh".to_string()),
                                scopes: Some(vec!["scope1".to_string(), "scope2".to_string()]),
                                flow: "flow".to_string(),
                            }
                        ))
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn custom_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.custom("mysec")
                    .data(json! ({
                        "hello": ["world", "mondo"],
                        "test": 1,
                    }))
                    .attype("ty1")
                    .attype("ty2")
                    .description("desc")
                    .descriptions(|ml| ml.add("en", "desc_en").add("it", "desc_it"))
                    .proxy("proxy")
                    .required()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["mysec".to_string()],
                security_definitions: [(
                    "mysec".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".to_string(), "desc_en".to_string()),
                                ("it".to_string(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Unknown(UnknownSecuritySchemeSubtype {
                            scheme: "mysec".to_string(),
                            data: json!({
                                "hello": ["world", "mondo"],
                                "test": 1,
                            })
                        })
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn named_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| b.no_sec().with_key("test_sec1").required())
            .security(|b| b.no_sec().with_key("test_sec2").required())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["test_sec1".to_string(), "test_sec2".to_string()],
                security_definitions: [
                    (
                        "test_sec1".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::NoSec
                            )
                        }
                    ),
                    (
                        "test_sec2".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::NoSec
                            )
                        }
                    )
                ]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn mixed_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| b.digest().with_key("sec1"))
            .security(|b| b.basic().with_key("sec2").required())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["sec2".to_string()],
                security_definitions: [
                    (
                        "sec1".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Digest(DigestSecurityScheme::default())
                            )
                        }
                    ),
                    (
                        "sec2".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Basic(BasicSecurityScheme::default())
                            )
                        }
                    ),
                ]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );

        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| b.digest())
            .security(|b| b.basic().required())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security: vec!["basic".to_string()],
                security_definitions: [
                    (
                        "digest".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Digest(DigestSecurityScheme::default())
                            )
                        }
                    ),
                    (
                        "basic".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Basic(BasicSecurityScheme::default())
                            )
                        }
                    ),
                ]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn colliding_security_names() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| b.basic())
            .security(|b| b.basic().required())
            .build()
            .unwrap_err();

        assert_eq!(
            err,
            Error::DuplicatedSecurityDefinition("basic".to_string())
        );
    }

    #[test]
    fn simple_form() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|form| form.href("href").op(FormOperation::ReadAllProperties))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    href: "href".to_string(),
                    ..Default::default()
                }]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn simple_form_with_uri_variables() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|form| form.href("href/{foo}").op(FormOperation::ReadAllProperties))
            .uri_variable("foo", |v| v.finish_extend().integer())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    href: "href/{foo}".to_string(),
                    ..Default::default()
                }]),
                uri_variables: Some(
                    [(
                        "foo".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::Integer(Default::default())),
                            ..Default::default()
                        }
                    )]
                    .into_iter()
                    .collect()
                ),
                ..Default::default()
            }
        );
    }
    #[test]
    fn complete_form() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|form| {
                form.href("href")
                    .op(FormOperation::ReadAllProperties)
                    .content_type("text/plain")
                    .content_coding("coding")
                    .subprotocol("subprotocol")
                    .security("digest")
                    .security("basic")
                    .scope("scope1")
                    .scope("scope2")
                    .response_default_ext("application/json")
            })
            .security(|b| b.digest())
            .security(|b| b.basic())
            .schema_definition("schema1", |b| b.finish_extend().bool())
            .schema_definition("schema2", |b| b.finish_extend().null())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    href: "href".to_string(),
                    content_type: Some("text/plain".into()),
                    content_coding: Some("coding".to_string()),
                    subprotocol: Some("subprotocol".to_string()),
                    security: Some(vec!["digest".to_string(), "basic".to_string()]),
                    scopes: Some(vec!["scope1".to_string(), "scope2".to_string()]),
                    response: Some(ExpectedResponse {
                        content_type: "application/json".to_string(),
                        other: Nil,
                    }),
                    other: Nil,
                }]),
                schema_definitions: Some(
                    [
                        (
                            "schema1".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Boolean),
                                ..Default::default()
                            }
                        ),
                        (
                            "schema2".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Null),
                                ..Default::default()
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                security_definitions: [
                    (
                        "digest".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Digest(DigestSecurityScheme::default())
                            )
                        }
                    ),
                    (
                        "basic".to_string(),
                        SecurityScheme {
                            attype: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            proxy: Default::default(),
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Basic(BasicSecurityScheme::default())
                            )
                        }
                    ),
                ]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn form_with_multiple_ops() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|form| {
                form.href("href")
                    .op(FormOperation::ReadAllProperties)
                    .op(FormOperation::ReadMultipleProperties)
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![
                        FormOperation::ReadAllProperties,
                        FormOperation::ReadMultipleProperties
                    ]),
                    href: "href".to_string(),
                    ..Default::default()
                }]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn invalid_form_without_op() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|form| form.href("href"))
            .build()
            .unwrap_err();

        assert_eq!(err, Error::MissingOpInForm);
    }

    #[test]
    fn invalid_form_with_invalid_op() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|form| form.href("href").op(FormOperation::ReadProperty))
            .build()
            .unwrap_err();

        assert_eq!(err, Error::InvalidOpInForm);
    }

    #[test]
    fn invalid_form_with_missing_security() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|form| {
                form.href("href")
                    .op(FormOperation::ReadAllProperties)
                    .security("basic")
            })
            .build()
            .unwrap_err();

        assert_eq!(err, Error::UndefinedSecurity("basic".to_string()));
    }

    #[test]
    fn with_property_affordance() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .property("on", |b| {
                b.finish_extend_data_schema()
                    .bool()
                    .observable(true)
                    .title("title")
            })
            .property("prop", |b| b.finish_extend_data_schema().null())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                properties: Some(
                    [
                        (
                            "on".to_owned(),
                            PropertyAffordance {
                                interaction: InteractionAffordance {
                                    attype: None,
                                    title: Some("title".to_owned()),
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    forms: vec![],
                                    uri_variables: None,
                                    other: Nil,
                                },
                                data_schema: DataSchema {
                                    attype: None,
                                    title: Some("title".to_owned()),
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
                                    subtype: Some(DataSchemaSubtype::Boolean),
                                    other: Nil,
                                },
                                observable: Some(true),
                                other: Nil,
                            }
                        ),
                        (
                            "prop".to_owned(),
                            PropertyAffordance {
                                interaction: InteractionAffordance {
                                    attype: None,
                                    title: None,
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    forms: vec![],
                                    uri_variables: None,
                                    other: Nil,
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
                                    subtype: Some(DataSchemaSubtype::Null),
                                    other: Nil,
                                },
                                observable: None,
                                other: Nil,
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Default::default()
            }
        );
    }

    #[test]
    fn with_action_affordance() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .action("fade", |b| b)
            .action("action", |b| {
                b.title("title")
                    .idempotent()
                    .input(|b| b.finish_extend().null())
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                actions: Some(
                    [
                        (
                            "fade".to_owned(),
                            ActionAffordance {
                                interaction: InteractionAffordance {
                                    attype: None,
                                    title: None,
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    forms: vec![],
                                    uri_variables: None,
                                    other: Nil,
                                },
                                input: None,
                                output: None,
                                safe: false,
                                idempotent: false,
                                synchronous: None,
                                other: Nil,
                            }
                        ),
                        (
                            "action".to_owned(),
                            ActionAffordance {
                                interaction: InteractionAffordance {
                                    attype: None,
                                    title: Some("title".to_owned()),
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    forms: vec![],
                                    uri_variables: None,
                                    other: Nil,
                                },
                                input: Some(DataSchema {
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
                                    subtype: Some(DataSchemaSubtype::Null),
                                    other: Nil,
                                }),
                                output: None,
                                safe: false,
                                idempotent: true,
                                synchronous: None,
                                other: Nil,
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Default::default()
            }
        );
    }

    #[test]
    fn with_event_affordance() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .event("overheat", |b| b)
            .event("event", |b| {
                b.title("title").cancellation(|b| b.finish_extend().null())
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                events: Some(
                    [
                        (
                            "overheat".to_owned(),
                            EventAffordance {
                                interaction: InteractionAffordance {
                                    attype: None,
                                    title: None,
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    forms: vec![],
                                    uri_variables: None,
                                    other: Nil,
                                },
                                subscription: None,
                                data: None,
                                cancellation: None,
                                data_response: None,
                                other: Nil,
                            }
                        ),
                        (
                            "event".to_owned(),
                            EventAffordance {
                                interaction: InteractionAffordance {
                                    attype: None,
                                    title: Some("title".to_owned()),
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    forms: vec![],
                                    uri_variables: None,
                                    other: Nil,
                                },
                                subscription: None,
                                data: None,
                                cancellation: Some(DataSchema {
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
                                    subtype: Some(DataSchemaSubtype::Null),
                                    other: Nil,
                                }),
                                data_response: None,
                                other: Nil,
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Default::default()
            }
        );
    }

    #[test]
    fn valid_affordance_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .property("on", |b| {
                b.finish_extend_data_schema()
                    .bool()
                    .form(|b| b.security("basic").href("href"))
            })
            .security(|b| b.basic())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                properties: Some(
                    [(
                        "on".to_owned(),
                        PropertyAffordance {
                            interaction: InteractionAffordance {
                                forms: vec![Form {
                                    op: DefaultedFormOperations::Default,
                                    href: "href".to_owned(),
                                    security: Some(vec!["basic".to_owned()]),
                                    ..Default::default()
                                }],
                                ..Default::default()
                            },
                            data_schema: DataSchema {
                                subtype: Some(DataSchemaSubtype::Boolean),
                                ..Default::default()
                            },
                            ..Default::default()
                        }
                    ),]
                    .into_iter()
                    .collect()
                ),
                security_definitions: [(
                    "basic".to_owned(),
                    SecurityScheme {
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Basic(
                            BasicSecurityScheme {
                                location: SecurityAuthenticationLocation::Header,
                                name: None,
                            }
                        )),
                        ..Default::default()
                    }
                )]
                .into_iter()
                .collect(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn invalid_affordance_security() {
        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .property("on", |b| {
                b.finish_extend_data_schema()
                    .bool()
                    .form(|b| b.security("oauth2").href("href"))
            })
            .security(|b| b.basic())
            .build()
            .unwrap_err();

        assert_eq!(error, Error::UndefinedSecurity("oauth2".to_owned()));
    }

    #[test]
    fn profile() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .profile("profile")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                profile: Some(vec!["profile".to_string()]),
                ..Default::default()
            }
        );

        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .profile("profile1")
            .profile("profile2")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                profile: Some(vec!["profile1".to_string(), "profile2".to_string()]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn schema_definitions() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .schema_definition("schema1", |b| b.finish_extend().null())
            .schema_definition("schema2", |b| b.finish_extend().number().minimum(5.))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                schema_definitions: Some(
                    [
                        (
                            "schema1".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Null),
                                ..Default::default()
                            },
                        ),
                        (
                            "schema2".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                                    minimum: Some(5.),
                                    ..Default::default()
                                })),
                                ..Default::default()
                            },
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Default::default()
            }
        );
    }

    #[test]
    fn extend_thing_with_form_builder() {
        #[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
        struct ThingA {}

        #[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
        struct ThingB {}

        #[derive(Debug, Serialize, PartialEq, Deserialize)]
        struct FormExtA {
            a: String,
        }

        #[derive(Debug, Serialize, PartialEq, Deserialize)]
        struct B(i32);

        #[derive(Debug, Serialize, PartialEq, Deserialize)]
        struct FormExtB {
            b: B,
        }

        impl ExtendableThing for ThingA {
            type InteractionAffordance = ();
            type PropertyAffordance = ();
            type ActionAffordance = ();
            type EventAffordance = ();
            type Form = FormExtA;
            type ExpectedResponse = ();
            type DataSchema = ();
            type ObjectSchema = ();
            type ArraySchema = ();
        }

        impl ExtendableThing for ThingB {
            type InteractionAffordance = ();
            type PropertyAffordance = ();
            type ActionAffordance = ();
            type EventAffordance = ();
            type Form = FormExtB;
            type ExpectedResponse = ();
            type DataSchema = ();
            type ObjectSchema = ();
            type ArraySchema = ();
        }

        let thing: Thing<Cons<ThingB, Cons<ThingA, Nil>>> =
            ThingBuilder::<Cons<ThingB, Cons<ThingA, Nil>>, _>::new("MyLampThing")
                .finish_extend()
                .form(|form| {
                    form.ext_with(|| FormExtA {
                        a: String::from("test"),
                    })
                    .href("href")
                    .ext(FormExtB { b: B(42) })
                    .op(FormOperation::ReadAllProperties)
                })
                .build()
                .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    href: "href".to_string(),
                    other: Cons::new_head(FormExtA {
                        a: "test".to_string()
                    })
                    .add(FormExtB { b: B(42) }),
                    content_type: Default::default(),
                    content_coding: Default::default(),
                    subprotocol: Default::default(),
                    security: Default::default(),
                    scopes: Default::default(),
                    response: Default::default(),
                }]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn extend_form_builder() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ThingA {}

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ThingB {}

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct A(String);

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct FormExtA {
            a: A,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ExpectedResponseExtA {
            b: A,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct B(i32);

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct FormExtB {
            c: B,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ExpectedResponseExtB {
            d: B,
        }

        impl ExtendableThing for ThingA {
            type InteractionAffordance = ();
            type PropertyAffordance = ();
            type ActionAffordance = ();
            type EventAffordance = ();
            type Form = FormExtA;
            type ExpectedResponse = ExpectedResponseExtA;
            type DataSchema = ();
            type ObjectSchema = ();
            type ArraySchema = ();
        }

        impl ExtendableThing for ThingB {
            type InteractionAffordance = ();
            type PropertyAffordance = ();
            type ActionAffordance = ();
            type EventAffordance = ();
            type Form = FormExtB;
            type ExpectedResponse = ExpectedResponseExtB;
            type DataSchema = ();
            type ObjectSchema = ();
            type ArraySchema = ();
        }

        let builder = FormBuilder::<Cons<ThingB, Cons<ThingA, Nil>>, _, _>::new()
            .href("href")
            .ext(FormExtA {
                a: A("a".to_string()),
            })
            .op(FormOperation::ReadProperty)
            .ext_with(|| FormExtB { c: B(1) })
            .response("application/json", |b| {
                b.ext(ExpectedResponseExtA {
                    b: A("b".to_string()),
                })
                .ext_with(|| ExpectedResponseExtB { d: B(2) })
            });

        let form: Form<Cons<ThingB, Cons<ThingA, Nil>>> = builder.into();
        assert_eq!(
            form,
            Form {
                op: DefaultedFormOperations::Custom(vec![FormOperation::ReadProperty]),
                href: "href".to_string(),
                other: Cons::new_head(FormExtA {
                    a: A("a".to_string())
                })
                .add(FormExtB { c: B(1) }),
                response: Some(ExpectedResponse {
                    content_type: "application/json".to_string(),
                    other: Cons::new_head(ExpectedResponseExtA {
                        b: A("b".to_string())
                    })
                    .add(ExpectedResponseExtB { d: B(2) })
                }),
                content_type: Default::default(),
                content_coding: Default::default(),
                subprotocol: Default::default(),
                security: Default::default(),
                scopes: Default::default(),
            },
        );
    }

    #[test]
    fn complete_extension() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ThingA {
            a: u8,
            b: i32,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ThingB {}

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ThingC {
            c: u16,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct InteractionAffordanceExtA {
            d: i16,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ActionAffordanceExtA {
            e: u64,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct EventAffordanceExtA {
            f: u32,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ExpectedResponseExtA {
            g: i64,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct DataSchemaExtA {
            h: isize,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ObjectSchemaExtA {
            i: usize,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct InteractionAffordanceExtB {
            j: f32,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct PropertyAffordanceExtB {
            k: f64,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct EventAffordanceExtB {
            l: i8,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct FormExtB {
            m: u8,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ExpectedResponseExtB {
            n: i16,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ObjectSchemaExtB {
            o: u16,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct InteractionAffordanceExtC {
            p: u64,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct PropertyAffordanceExtC {
            q: i8,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ActionAffordanceExtC {
            r: i32,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ExpectedResponseExtC {
            s: u8,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct DataSchemaExtC {
            t: u32,
        }

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct ObjectSchemaExtC {
            u: i32,
        }

        impl ExtendableThing for ThingA {
            type InteractionAffordance = InteractionAffordanceExtA;
            type PropertyAffordance = ();
            type ActionAffordance = ActionAffordanceExtA;
            type EventAffordance = EventAffordanceExtA;
            type Form = ();
            type ExpectedResponse = ExpectedResponseExtA;
            type DataSchema = DataSchemaExtA;
            type ObjectSchema = ObjectSchemaExtA;
            type ArraySchema = ();
        }

        impl ExtendableThing for ThingB {
            type InteractionAffordance = InteractionAffordanceExtB;
            type PropertyAffordance = PropertyAffordanceExtB;
            type ActionAffordance = ();
            type EventAffordance = EventAffordanceExtB;
            type Form = FormExtB;
            type ExpectedResponse = ExpectedResponseExtB;
            type DataSchema = ();
            type ObjectSchema = ObjectSchemaExtB;
            type ArraySchema = ();
        }

        impl ExtendableThing for ThingC {
            type InteractionAffordance = InteractionAffordanceExtC;
            type PropertyAffordance = PropertyAffordanceExtC;
            type ActionAffordance = ActionAffordanceExtC;
            type EventAffordance = ();
            type Form = ();
            type ExpectedResponse = ExpectedResponseExtC;
            type DataSchema = DataSchemaExtC;
            type ObjectSchema = ObjectSchemaExtC;
            type ArraySchema = ();
        }

        let thing = Thing::build("thing title")
            .ext(ThingA { a: 1, b: 2 })
            .id("id")
            .ext(ThingB {})
            .ext_with(|| ThingC { c: 3 })
            .finish_extend()
            .description("description")
            .uri_variable("uri_variable", |b| {
                b.ext(DataSchemaExtA { h: 4 })
                    .ext(())
                    .ext(DataSchemaExtC { t: 5 })
                    .finish_extend()
                    .array()
            })
            .property("property", |b| {
                b.ext_interaction(InteractionAffordanceExtA { d: 6 })
                    .ext(())
                    .ext_data_schema(DataSchemaExtA { h: 7 })
                    .ext_data_schema(())
                    .ext_data_schema(DataSchemaExtC { t: 8 })
                    .finish_extend_data_schema()
                    .ext_interaction(InteractionAffordanceExtB { j: 9. })
                    .ext_interaction(InteractionAffordanceExtC { p: 10 })
                    .object_ext(|b| {
                        b.ext(ObjectSchemaExtA { i: 11 })
                            .ext(ObjectSchemaExtB { o: 12 })
                            .ext(ObjectSchemaExtC { u: 13 })
                    })
                    .ext(PropertyAffordanceExtB { k: 14. })
                    .ext(PropertyAffordanceExtC { q: 15 })
                    .form(|b| {
                        b.response("application/json", |b| {
                            b.ext(ExpectedResponseExtA { g: 16 })
                                .ext(ExpectedResponseExtB { n: 17 })
                                .ext(ExpectedResponseExtC { s: 18 })
                        })
                        .ext(())
                        .ext(FormExtB { m: 19 })
                        .ext(())
                        .href("href1")
                    })
            })
            .action("action", |b| {
                b.ext(ActionAffordanceExtA { e: 20 })
                    .ext(())
                    .ext(ActionAffordanceExtC { r: 21 })
                    .ext_interaction(InteractionAffordanceExtA { d: 22 })
                    .ext_interaction(InteractionAffordanceExtB { j: 23. })
                    .ext_interaction(InteractionAffordanceExtC { p: 24 })
                    .input(|b| {
                        b.ext(DataSchemaExtA { h: 25 })
                            .ext(())
                            .ext(DataSchemaExtC { t: 26 })
                            .finish_extend()
                            .number()
                            .minimum(0.)
                            .maximum(5.)
                            .title("input")
                    })
                    .uri_variable("y", |b| {
                        b.ext(DataSchemaExtA { h: 27 })
                            .ext(())
                            .ext(DataSchemaExtC { t: 28 })
                            .finish_extend()
                            .array_ext(|b| b.ext(()).ext(()).ext(()))
                    })
                    .title("action")
            })
            .event("event", |b| {
                b.ext(EventAffordanceExtA { f: 29 })
                    .ext(EventAffordanceExtB { l: 30 })
                    .ext(())
                    .ext_interaction(InteractionAffordanceExtA { d: 31 })
                    .ext_interaction(InteractionAffordanceExtB { j: 32. })
                    .ext_interaction(InteractionAffordanceExtC { p: 33 })
                    .data(|b| {
                        b.ext(DataSchemaExtA { h: 34 })
                            .ext(())
                            .ext(DataSchemaExtC { t: 35 })
                            .finish_extend()
                            .bool()
                    })
            })
            .form(|b| {
                b.ext(())
                    .ext(FormExtB { m: 36 })
                    .ext(())
                    .href("href2")
                    .response("test", |b| {
                        b.ext(ExpectedResponseExtA { g: 37 })
                            .ext(ExpectedResponseExtB { n: 38 })
                            .ext(ExpectedResponseExtC { s: 39 })
                    })
                    .op(FormOperation::ReadAllProperties)
            })
            .schema_definition("schema", |b| {
                b.ext(DataSchemaExtA { h: 40 })
                    .ext(())
                    .ext(DataSchemaExtC { t: 41 })
                    .finish_extend()
                    .null()
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "thing title".to_string(),
                other: Cons::new_head(ThingA { a: 1, b: 2 })
                    .add(ThingB {})
                    .add(ThingC { c: 3 }),
                id: Some("id".to_string()),
                description: Some("description".to_string()),
                uri_variables: Some(
                    [(
                        "uri_variable".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::Array(ArraySchema::default())),
                            other: Cons::new_head(DataSchemaExtA { h: 4 })
                                .add(())
                                .add(DataSchemaExtC { t: 5 }),
                            attype: Default::default(),
                            title: Default::default(),
                            titles: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            constant: Default::default(),
                            unit: Default::default(),
                            one_of: Default::default(),
                            enumeration: Default::default(),
                            read_only: Default::default(),
                            write_only: Default::default(),
                            format: Default::default(),
                        }
                    )]
                    .into_iter()
                    .collect()
                ),
                properties: Some(
                    [(
                        "property".to_string(),
                        PropertyAffordance {
                            interaction: InteractionAffordance {
                                other: Cons::new_head(InteractionAffordanceExtA { d: 6 })
                                    .add(InteractionAffordanceExtB { j: 9. })
                                    .add(InteractionAffordanceExtC { p: 10 }),
                                attype: Default::default(),
                                title: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                forms: vec![Form {
                                    href: "href1".to_string(),
                                    response: Some(ExpectedResponse {
                                        content_type: "application/json".to_string(),
                                        other: Cons::new_head(ExpectedResponseExtA { g: 16 })
                                            .add(ExpectedResponseExtB { n: 17 })
                                            .add(ExpectedResponseExtC { s: 18 })
                                    }),
                                    other: Cons::new_head(()).add(FormExtB { m: 19 }).add(()),
                                    op: Default::default(),
                                    content_type: Default::default(),
                                    content_coding: Default::default(),
                                    subprotocol: Default::default(),
                                    security: Default::default(),
                                    scopes: Default::default(),
                                }],
                                uri_variables: Default::default(),
                            },
                            data_schema: DataSchema {
                                subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                                    other: Cons::new_head(ObjectSchemaExtA { i: 11 })
                                        .add(ObjectSchemaExtB { o: 12 })
                                        .add(ObjectSchemaExtC { u: 13 }),
                                    properties: Default::default(),
                                    required: Default::default(),
                                })),
                                other: Cons::new_head(DataSchemaExtA { h: 7 })
                                    .add(())
                                    .add(DataSchemaExtC { t: 8 }),
                                attype: Default::default(),
                                title: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                constant: Default::default(),
                                unit: Default::default(),
                                one_of: Default::default(),
                                enumeration: Default::default(),
                                read_only: Default::default(),
                                write_only: Default::default(),
                                format: Default::default(),
                            },
                            other: Cons::new_head(())
                                .add(PropertyAffordanceExtB { k: 14. })
                                .add(PropertyAffordanceExtC { q: 15 }),
                            observable: Default::default(),
                        }
                    )]
                    .into_iter()
                    .collect()
                ),
                actions: Some(
                    [(
                        "action".to_string(),
                        ActionAffordance {
                            interaction: InteractionAffordance {
                                title: Some("action".to_string()),
                                uri_variables: Some(
                                    [(
                                        "y".to_string(),
                                        DataSchema {
                                            subtype: Some(DataSchemaSubtype::Array(
                                                ArraySchema::default()
                                            )),
                                            other: Cons::new_head(DataSchemaExtA { h: 27 })
                                                .add(())
                                                .add(DataSchemaExtC { t: 28 }),
                                            attype: Default::default(),
                                            title: Default::default(),
                                            titles: Default::default(),
                                            description: Default::default(),
                                            descriptions: Default::default(),
                                            constant: Default::default(),
                                            unit: Default::default(),
                                            one_of: Default::default(),
                                            enumeration: Default::default(),
                                            read_only: Default::default(),
                                            write_only: Default::default(),
                                            format: Default::default(),
                                        }
                                    )]
                                    .into_iter()
                                    .collect()
                                ),
                                other: Cons::new_head(InteractionAffordanceExtA { d: 22 })
                                    .add(InteractionAffordanceExtB { j: 23. })
                                    .add(InteractionAffordanceExtC { p: 24 }),
                                attype: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                forms: Default::default(),
                            },
                            input: Some(DataSchema {
                                title: Some("input".to_string()),
                                subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                                    minimum: Some(0.),
                                    maximum: Some(5.),
                                    ..Default::default()
                                })),
                                other: Cons::new_head(DataSchemaExtA { h: 25 })
                                    .add(())
                                    .add(DataSchemaExtC { t: 26 }),
                                attype: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                constant: Default::default(),
                                unit: Default::default(),
                                one_of: Default::default(),
                                enumeration: Default::default(),
                                read_only: Default::default(),
                                write_only: Default::default(),
                                format: Default::default(),
                            }),
                            other: Cons::new_head(ActionAffordanceExtA { e: 20 })
                                .add(())
                                .add(ActionAffordanceExtC { r: 21 }),
                            output: Default::default(),
                            safe: Default::default(),
                            idempotent: Default::default(),
                            synchronous: Default::default(),
                        }
                    )]
                    .into_iter()
                    .collect()
                ),
                events: Some(
                    [(
                        "event".to_string(),
                        EventAffordance {
                            interaction: InteractionAffordance {
                                other: Cons::new_head(InteractionAffordanceExtA { d: 31 })
                                    .add(InteractionAffordanceExtB { j: 32. })
                                    .add(InteractionAffordanceExtC { p: 33 }),
                                attype: Default::default(),
                                title: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                forms: Default::default(),
                                uri_variables: Default::default(),
                            },
                            data: Some(DataSchema {
                                subtype: Some(DataSchemaSubtype::Boolean),
                                other: Cons::new_head(DataSchemaExtA { h: 34 })
                                    .add(())
                                    .add(DataSchemaExtC { t: 35 }),
                                attype: Default::default(),
                                title: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                constant: Default::default(),
                                unit: Default::default(),
                                one_of: Default::default(),
                                enumeration: Default::default(),
                                read_only: Default::default(),
                                write_only: Default::default(),
                                format: Default::default(),
                            }),
                            other: Cons::new_head(EventAffordanceExtA { f: 29 })
                                .add(EventAffordanceExtB { l: 30 })
                                .add(()),
                            subscription: Default::default(),
                            data_response: Default::default(),
                            cancellation: Default::default(),
                        }
                    )]
                    .into_iter()
                    .collect()
                ),
                forms: Some(vec![Form {
                    href: "href2".to_string(),
                    response: Some(ExpectedResponse {
                        content_type: "test".to_string(),
                        other: Cons::new_head(ExpectedResponseExtA { g: 37 })
                            .add(ExpectedResponseExtB { n: 38 })
                            .add(ExpectedResponseExtC { s: 39 })
                    }),
                    other: Cons::new_head(()).add(FormExtB { m: 36 }).add(()),
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    content_type: Default::default(),
                    content_coding: Default::default(),
                    subprotocol: Default::default(),
                    security: Default::default(),
                    scopes: Default::default(),
                }]),
                schema_definitions: Some(
                    [(
                        "schema".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::Null),
                            other: Cons::new_head(DataSchemaExtA { h: 40 })
                                .add(())
                                .add(DataSchemaExtC { t: 41 }),
                            attype: Default::default(),
                            title: Default::default(),
                            titles: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            constant: Default::default(),
                            unit: Default::default(),
                            one_of: Default::default(),
                            enumeration: Default::default(),
                            read_only: Default::default(),
                            write_only: Default::default(),
                            format: Default::default(),
                        }
                    )]
                    .into_iter()
                    .collect()
                ),
                attype: Default::default(),
                titles: Default::default(),
                descriptions: Default::default(),
                version: Default::default(),
                created: Default::default(),
                modified: Default::default(),
                support: Default::default(),
                base: Default::default(),
                links: Default::default(),
                security: Default::default(),
                security_definitions: Default::default(),
                profile: Default::default(),
            },
        );
    }
}
