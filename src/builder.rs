//! Thing Description builder
//!
//! The main entry point is [ThingBuilder].
//!
//! TODO: Write an example usage

pub mod affordance;
pub mod data_schema;
pub mod human_readable_info;

use std::{borrow::Cow, collections::HashMap, fmt, marker::PhantomData, ops::Not};

use oxilangtag::LanguageTag;
use serde_json::Value;
use time::OffsetDateTime;

use crate::{
    builder::data_schema::uri_variables_contains_arrays_objects,
    extend::{Extend, Extendable, ExtendableThing},
    thing::{
        AdditionalExpectedResponse, ApiKeySecurityScheme, BasicSecurityScheme,
        BearerSecurityScheme, ComboSecurityScheme, DataSchemaFromOther, DefaultedFormOperations,
        DigestSecurityScheme, ExpectedResponse, Form, FormOperation, KnownSecuritySchemeSubtype,
        Link, OAuth2SecurityScheme, PskSecurityScheme, QualityOfProtection,
        SecurityAuthenticationLocation, SecurityScheme, SecuritySchemeSubtype, Thing,
        UnknownSecuritySchemeSubtype, VersionInfo, TD_CONTEXT_11,
    },
};

use self::{
    affordance::{
        ActionAffordanceBuilder, AffordanceBuilder, BuildableAffordance,
        CheckableInteractionAffordanceBuilder, EventAffordanceBuilder, IntoUsable,
        PropertyAffordanceBuilder, UsableActionAffordanceBuilder, UsableEventAffordanceBuilder,
        UsablePropertyAffordanceBuilder,
    },
    data_schema::{
        CheckableDataSchema, DataSchemaBuilder, PartialDataSchemaBuilder,
        UncheckedDataSchemaFromOther,
    },
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ToExtend;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Extended;

/// A builder for a [Thing]
#[must_use]
pub struct ThingBuilder<Other: ExtendableThing, Status> {
    context: Vec<Context>,
    id: Option<String>,
    attype: Option<Vec<String>>,
    title: String,
    titles: Option<MultiLanguageBuilder<String>>,
    description: Option<String>,
    descriptions: Option<MultiLanguageBuilder<String>>,
    version: Option<VersionInfo>,
    created: Option<OffsetDateTime>,
    modified: Option<OffsetDateTime>,
    support: Option<String>,
    base: Option<String>,
    properties: Vec<AffordanceBuilder<UsablePropertyAffordanceBuilder<Other>>>,
    actions: Vec<AffordanceBuilder<UsableActionAffordanceBuilder<Other>>>,
    events: Vec<AffordanceBuilder<UsableEventAffordanceBuilder<Other>>>,
    links: Option<Vec<UncheckedLink>>,
    forms: Option<Vec<FormBuilder<Other, String, Other::Form>>>,
    uri_variables: Option<HashMap<String, UncheckedDataSchemaFromOther<Other>>>,
    security: Vec<String>,
    security_definitions: Vec<(String, UncheckedSecurityScheme)>,
    profile: Vec<String>,
    schema_definitions: HashMap<String, UncheckedDataSchemaFromOther<Other>>,
    pub other: Other,
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

    /// The Form can use only a specific set of operations depending on the context.
    #[error("Invalid Form operation {operation} in {context} context")]
    InvalidOpInForm {
        context: FormContext,
        operation: FormOperation,
    },

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

    #[error("Using the data schema \"{0}\", which is not declared in the schema definitions")]
    MissingSchemaDefinition(String),

    #[error("An uriVariable cannot be an ObjectSchema or ArraySchema")]
    InvalidUriVariables,

    #[error("Invalid language tag \"{0}\"")]
    InvalidLanguageTag(String),

    #[error("A sizes field can be used only when \"rel\" is \"icon\"")]
    SizesWithRelNotIcon,
}

/// Context of a [`Form`]
///
/// [`Form`]: `crate::thing::Form`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FormContext {
    /// The root Thing context
    Thing,

    /// A property affordance context
    Property,

    /// An action affordance context
    Action,

    /// An event affordance context
    Event,
}

impl fmt::Display for FormContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Thing => "Thing",
            Self::Property => "PropertyAffordance",
            Self::Action => "ActionAffordance",
            Self::Event => "EventAffordance",
        };

        f.write_str(s)
    }
}

impl From<AffordanceType> for FormContext {
    fn from(ty: AffordanceType) -> Self {
        match ty {
            AffordanceType::Property => Self::Property,
            AffordanceType::Action => Self::Action,
            AffordanceType::Event => Self::Event,
        }
    }
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

    /// Finalize the set of extensions that must be populated
    ///
    /// Moves the builder status from [ToExtend] to [Extended].
    /// From this point is not possible to add further extensions to the builder.
    ///
    /// See [ThingBuilder::ext].
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

    /// Extend the [ThingBuilder] with a [ExtendableThing]
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
            let scheme: SecurityScheme = scheme.try_into()?;

            match security_definitions.entry(name) {
                Entry::Vacant(entry) => {
                    entry.insert(scheme);
                }
                Entry::Occupied(entry) => {
                    return Err(Error::DuplicatedSecurityDefinition(entry.remove_entry().0));
                }
            }
        }
        let security_definitions = security_definitions;
        security_definitions
            .values()
            .filter_map(|security| match &security.subtype {
                SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Combo(combo)) => {
                    Some(combo)
                }
                _ => None,
            })
            .flat_map(|combo| match combo {
                ComboSecurityScheme::OneOf(names) => names.as_slice(),
                ComboSecurityScheme::AllOf(names) => names.as_slice(),
            })
            .try_for_each(|security_name| {
                security_definitions
                    .contains_key(security_name)
                    .then_some(())
                    .ok_or_else(|| Error::MissingSchemaDefinition(security_name.to_string()))
            })?;
        let schema_definitions = schema_definitions
            .into_iter()
            .map(|(key, value)| value.try_into().map(|value| (key, value)))
            .collect::<Result<_, _>>()?;

        let profile = profile.is_empty().not().then_some(profile);

        let forms = forms
            .map(|forms| {
                forms
                    .into_iter()
                    .map(|form_builder| {
                        Self::build_form_from_builder(
                            form_builder,
                            &security_definitions,
                            &schema_definitions,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?;

        let schema_definitions = schema_definitions
            .is_empty()
            .not()
            .then_some(schema_definitions);

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

        let invalid_uri_variables = uri_variables
            .as_ref()
            .map(uri_variables_contains_arrays_objects::<Other>)
            .unwrap_or(false);
        if invalid_uri_variables {
            return Err(Error::InvalidUriVariables);
        }

        let uri_variables = uri_variables
            .map(|uri_variables| {
                uri_variables
                    .into_iter()
                    .map(|(key, value)| value.try_into().map(|value| (key, value)))
                    .collect()
            })
            .transpose()?;

        let properties = try_build_affordance(
            properties,
            AffordanceType::Property,
            |property| &property.interaction,
            |property| [Some(&property.data_schema)],
            |op| {
                matches!(
                    op,
                    FormOperation::ReadProperty
                        | FormOperation::WriteProperty
                        | FormOperation::ObserveProperty
                        | FormOperation::UnobserveProperty
                )
            },
            &security_definitions,
        )?;
        let actions = try_build_affordance(
            actions,
            AffordanceType::Action,
            |action| &action.interaction,
            |action| [action.input.as_ref(), action.output.as_ref()],
            |op| {
                matches!(
                    op,
                    FormOperation::InvokeAction
                        | FormOperation::QueryAction
                        | FormOperation::CancelAction
                )
            },
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
            |op| {
                matches!(
                    op,
                    FormOperation::SubscribeEvent | FormOperation::UnsubscribeEvent
                )
            },
            &security_definitions,
        )?;
        let links = links
            .map(|links| links.into_iter().map(TryInto::try_into).collect())
            .transpose()?;

        let titles = titles.map(|titles| titles.build()).transpose()?;
        let descriptions = descriptions
            .map(|descriptions| descriptions.build())
            .transpose()?;

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
        schema_definitions: &HashMap<String, DataSchemaFromOther<Other>>,
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
            additional_responses,
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
                let wrong_op = operations
                    .iter()
                    .find(|op| {
                        matches!(
                            op,
                            ReadAllProperties
                                | WriteAllProperties
                                | ReadMultipleProperties
                                | WriteMultipleProperties
                                | ObserveAllProperties
                                | UnobserveAllProperties
                                | SubscribeAllEvents
                                | UnsubscribeAllEvents
                                | QueryAllActions
                        )
                        .not()
                    })
                    .copied();

                if let Some(operation) = wrong_op {
                    return Err(Error::InvalidOpInForm {
                        context: FormContext::Thing,
                        operation,
                    });
                }
            }
        }

        additional_responses
            .iter()
            .flat_map(|additional_response| additional_response.schema.as_ref())
            .try_for_each(|schema| {
                schema_definitions
                    .contains_key(schema)
                    .then_some(())
                    .ok_or_else(|| Error::MissingSchemaDefinition(schema.clone()))
            })?;
        let additional_responses = additional_responses
            .is_empty()
            .not()
            .then_some(additional_responses);

        Ok(Form {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            additional_responses,
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

        self.titles = Some(builder);
        self
    }

    /// Set multi-language descriptions
    pub fn descriptions<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);
        self.descriptions = Some(builder);
        self
    }

    /// Add an additional link to the Thing Description
    pub fn link(mut self, href: impl Into<String>) -> Self {
        let href = href.into();

        let link = UncheckedLink {
            href,
            ty: Default::default(),
            rel: Default::default(),
            anchor: Default::default(),
            sizes: Default::default(),
            hreflang: Default::default(),
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
            sizes,
            hreflang,
        } = f(LinkBuilder::new());

        let link = UncheckedLink {
            href,
            ty,
            rel,
            anchor,
            sizes,
            hreflang,
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
        let security_scheme = UncheckedSecurityScheme {
            attype,
            description,
            descriptions,
            proxy,
            subtype,
        };

        let name = name.unwrap_or_else(|| {
            match &security_scheme.subtype {
                Known(KnownSecuritySchemeSubtype::NoSec) => "nosec",
                Known(KnownSecuritySchemeSubtype::Auto) => "auto",
                Known(KnownSecuritySchemeSubtype::Combo(_)) => "combo",
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
        T: Into<UncheckedDataSchemaFromOther<Other>>,
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
        T: IntoUsable<UsablePropertyAffordanceBuilder<Other>>,
        Other::DataSchema: Extendable,
        Other::InteractionAffordance: Extendable,
        Other::PropertyAffordance: Extendable,
    {
        let affordance = f(PropertyAffordanceBuilder::empty()).into_usable();
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
        T: Into<UncheckedDataSchemaFromOther<Other>>,
        Other::DataSchema: Extendable,
    {
        self.schema_definitions.insert(
            name.into(),
            f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into(),
        );
        self
    }
}

fn try_build_affordance<A, F, IA, G, DS, T, H, const N: usize>(
    affordances: Vec<AffordanceBuilder<A>>,
    affordance_type: AffordanceType,
    mut get_interaction: F,
    mut get_data_schemas: G,
    is_allowed_op: H,
    security_definitions: &HashMap<String, SecurityScheme>,
) -> Result<Option<HashMap<String, T>>, Error>
where
    F: FnMut(&A) -> &IA,
    IA: CheckableInteractionAffordanceBuilder,
    G: FnMut(&A) -> [Option<&DS>; N],
    DS: CheckableDataSchema,
    A: BuildableAffordance<Target = T>,
    H: Fn(FormOperation) -> bool,
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

                    get_interaction(&affordance).check(
                        security_definitions,
                        affordance_type,
                        &is_allowed_op,
                    )?;
                    get_data_schemas(&affordance)
                        .into_iter()
                        .flatten()
                        .try_for_each(CheckableDataSchema::check)?;

                    match affordances.entry(name) {
                        Entry::Vacant(entry) => {
                            entry.insert(affordance.build()?);
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
#[derive(Clone, Debug, Default, PartialEq, Eq)]
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

    pub(crate) fn build(self) -> Result<HashMap<LanguageTag<String>, T>, Error> {
        self.values
            .into_iter()
            .map(|(k, v)| {
                // See https://github.com/oxigraph/oxilangtag/issues/4 for the reason of this,
                // which unnecessarily allocate.
                k.parse()
                    .map(|k| (k, v))
                    .map_err(|_| Error::InvalidLanguageTag(k))
            })
            .collect()
    }
}

/// Builder for Thing Description Links
pub struct LinkBuilder<Href> {
    href: Href,
    ty: Option<String>,
    rel: Option<String>,
    anchor: Option<String>,
    sizes: Option<String>,
    hreflang: Vec<String>,
}

impl LinkBuilder<()> {
    const fn new() -> Self {
        Self {
            href: (),
            ty: None,
            rel: None,
            anchor: None,
            sizes: None,
            hreflang: Vec::new(),
        }
    }

    /// Create a builder with the defined href
    pub fn href(self, value: impl Into<String>) -> LinkBuilder<String> {
        let Self {
            href: (),
            ty,
            rel,
            anchor,
            sizes,
            hreflang,
        } = self;

        let href = value.into();
        LinkBuilder {
            href,
            ty,
            rel,
            anchor,
            sizes,
            hreflang,
        }
    }
}

impl<T> LinkBuilder<T> {
    opt_field_builder!(ty: String, rel: String, anchor: String, sizes: String);

    /// Appends an hreflang parameter that will be checked in the call to [`ThingBuilder::build`].
    pub fn hreflang(mut self, value: impl Into<String>) -> Self {
        self.hreflang.push(value.into());
        self
    }
}

/// Builder for the Security Scheme
pub struct SecuritySchemeBuilder<S> {
    attype: Option<Vec<String>>,
    description: Option<String>,
    descriptions: Option<MultiLanguageBuilder<String>>,
    proxy: Option<String>,
    name: Option<String>,
    subtype: S,
    required: bool,
}

/// Placeholder Type for the NoSecurity Scheme
pub struct SecuritySchemeNoSecTag;

/// Placeholder Type for the Auto Security Scheme
pub struct SecuritySchemeAutoTag;

/// Placeholder Type for the Combo Security Scheme without `allOf` or `oneOf`
pub struct EmptyComboSecuritySchemeTag;

/// Placeholder Type for the Combo Security Scheme using `allOf`
#[derive(Debug, Default, PartialEq, Eq)]
pub struct AllOfComboSecuritySchemeTag;

/// Placeholder Type for the Combo Security Scheme using `oneOf`
#[derive(Debug, Default, PartialEq, Eq)]
pub struct OneOfComboSecuritySchemeTag;

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

    /// Auto security scheme
    pub fn auto(self) -> SecuritySchemeBuilder<SecuritySchemeAutoTag> {
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
            subtype: SecuritySchemeAutoTag,
            required,
        }
    }

    /// Combo security scheme
    pub fn combo(self) -> SecuritySchemeBuilder<EmptyComboSecuritySchemeTag> {
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
            subtype: EmptyComboSecuritySchemeTag,
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
        self.descriptions = Some(builder);
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

impl BuildableSecuritySchemeSubtype for SecuritySchemeAutoTag {
    fn build(self) -> SecuritySchemeSubtype {
        SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Auto)
    }
}

impl BuildableSecuritySchemeSubtype for (AllOfComboSecuritySchemeTag, Vec<String>) {
    fn build(self) -> SecuritySchemeSubtype {
        SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Combo(
            ComboSecurityScheme::AllOf(self.1),
        ))
    }
}

impl BuildableSecuritySchemeSubtype for (OneOfComboSecuritySchemeTag, Vec<String>) {
    fn build(self) -> SecuritySchemeSubtype {
        SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Combo(
            ComboSecurityScheme::OneOf(self.1),
        ))
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

impl SecuritySchemeBuilder<EmptyComboSecuritySchemeTag> {
    /// Require all the specified schema definitions for the security combo.
    pub fn all_of<I, T>(
        self,
        iter: I,
    ) -> SecuritySchemeBuilder<(AllOfComboSecuritySchemeTag, Vec<String>)>
    where
        I: IntoIterator<Item = T>,
        T: Into<String>,
    {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;
        let subtype = (AllOfComboSecuritySchemeTag, Vec::new());

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype,
            required,
        }
        .extend(iter)
    }

    /// Require one of the specified schema definitions for the security combo.
    pub fn one_of<I, T>(
        self,
        iter: I,
    ) -> SecuritySchemeBuilder<(OneOfComboSecuritySchemeTag, Vec<String>)>
    where
        I: IntoIterator<Item = T>,
        T: Into<String>,
    {
        let Self {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype: _,
            required,
        } = self;
        let subtype = (OneOfComboSecuritySchemeTag, Vec::new());

        SecuritySchemeBuilder {
            attype,
            description,
            descriptions,
            proxy,
            name,
            subtype,
            required,
        }
        .extend(iter)
    }

    /// Name for query, header or cookie parameter
    pub fn name(mut self, value: impl Into<String>) -> Self {
        self.name = Some(value.into());
        self
    }
}

impl<T> SecuritySchemeBuilder<(T, Vec<String>)> {
    pub fn extend<I, U>(mut self, iter: I) -> Self
    where
        I: IntoIterator<Item = U>,
        U: Into<String>,
    {
        self.subtype.1.extend(iter.into_iter().map(Into::into));
        self
    }

    pub fn push(mut self, security_name: impl Into<String>) -> Self {
        self.subtype.1.push(security_name.into());
        self
    }

    /// Name for query, header or cookie parameter
    pub fn name(mut self, value: impl Into<String>) -> Self {
        self.name = Some(value.into());
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
    additional_responses: Vec<AdditionalExpectedResponse>,
    pub other: OtherForm,
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
            additional_responses: Default::default(),
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
            additional_responses,
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
            additional_responses,
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

    pub fn additional_response<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut AdditionalExpectedResponseBuilder) -> &mut AdditionalExpectedResponseBuilder,
    {
        let mut builder = AdditionalExpectedResponseBuilder::new();
        f(&mut builder);

        let AdditionalExpectedResponseBuilder {
            success,
            content_type,
            schema,
        } = builder;
        let additional_response = AdditionalExpectedResponse {
            success,
            content_type,
            schema,
        };

        self.additional_responses.push(additional_response);
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
            additional_responses,
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
            additional_responses,
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
            additional_responses,
            other,
            _marker: _,
        } = builder;

        let additional_responses = additional_responses
            .is_empty()
            .not()
            .then_some(additional_responses);

        Self {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
            additional_responses,
            other,
        }
    }
}

/// Builder for the AdditionalExpectedResponse
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AdditionalExpectedResponseBuilder {
    success: bool,
    content_type: Option<String>,
    schema: Option<String>,
}

impl AdditionalExpectedResponseBuilder {
    const fn new() -> Self {
        Self {
            success: false,
            content_type: None,
            schema: None,
        }
    }

    pub fn success(&mut self) -> &mut Self {
        self.success = true;
        self
    }

    pub fn content_type(&mut self, value: impl Into<String>) -> &mut Self {
        self.content_type = Some(value.into());
        self
    }

    pub fn schema(&mut self, value: impl Into<String>) -> &mut Self {
        self.schema = Some(value.into());
        self
    }
}

pub(crate) struct UncheckedSecurityScheme {
    attype: Option<Vec<String>>,
    description: Option<String>,
    descriptions: Option<MultiLanguageBuilder<String>>,
    proxy: Option<String>,
    subtype: SecuritySchemeSubtype,
}

impl TryFrom<UncheckedSecurityScheme> for SecurityScheme {
    type Error = Error;

    fn try_from(scheme: UncheckedSecurityScheme) -> Result<Self, Self::Error> {
        let UncheckedSecurityScheme {
            attype,
            description,
            descriptions,
            proxy,
            subtype,
        } = scheme;

        let descriptions = descriptions
            .map(|descriptions| descriptions.build())
            .transpose()?;

        Ok(Self {
            attype,
            description,
            descriptions,
            proxy,
            subtype,
        })
    }
}

pub struct UncheckedLink {
    href: String,
    ty: Option<String>,
    rel: Option<String>,
    anchor: Option<String>,
    sizes: Option<String>,
    hreflang: Vec<String>,
}

impl TryFrom<UncheckedLink> for Link {
    type Error = Error;

    fn try_from(link: UncheckedLink) -> Result<Self, Self::Error> {
        let UncheckedLink {
            href,
            ty,
            rel,
            anchor,
            sizes,
            hreflang,
        } = link;

        if sizes.is_some() && rel.as_deref() != Some("icon") {
            return Err(Error::SizesWithRelNotIcon);
        }

        let hreflang = hreflang
            .into_iter()
            .map(|lang| lang.parse().map_err(|_| Error::InvalidLanguageTag(lang)))
            .collect::<Result<Vec<_>, _>>()?;
        let hreflang = hreflang.is_empty().not().then_some(hreflang);

        Ok(Self {
            href,
            ty,
            rel,
            anchor,
            sizes,
            hreflang,
        })
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
            ActionAffordance, DataSchema, DataSchemaSubtype, EventAffordance,
            InteractionAffordance, Maximum, Minimum, NumberSchema, ObjectSchema,
            PropertyAffordance, StringSchema,
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
                        .map(|(k, v)| (k.parse().unwrap(), v.to_string()))
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
                        .map(|(k, v)| (k.parse().unwrap(), v.to_string()))
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
                        sizes: Default::default(),
                        hreflang: Default::default(),
                    },
                    Link {
                        href: "href2".to_string(),
                        ty: Default::default(),
                        rel: Default::default(),
                        anchor: Default::default(),
                        sizes: Default::default(),
                        hreflang: Default::default(),
                    }
                ]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn link_with() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .link_with(|link| {
                link.href("href1")
                    .ty("ty")
                    .rel("icon")
                    .anchor("anchor")
                    .sizes("10x20 30x50")
                    .hreflang("it")
                    .hreflang("en")
            })
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
                        rel: Some("icon".to_string()),
                        anchor: Some("anchor".to_string()),
                        sizes: Some("10x20 30x50".to_string()),
                        hreflang: Some(vec!["it".parse().unwrap(), "en".parse().unwrap()]),
                    },
                    Link {
                        href: "href2".to_string(),
                        ty: Default::default(),
                        rel: Default::default(),
                        anchor: Default::default(),
                        sizes: Default::default(),
                        hreflang: Default::default(),
                    }
                ]),
                ..Default::default()
            }
        );
    }

    #[test]
    fn invalid_link_sizes_without_type_icon() {
        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .link_with(|link| link.href("href1").rel("other").sizes("10x20 30x50"))
            .build()
            .unwrap_err();

        assert_eq!(error, Error::SizesWithRelNotIcon);
    }

    #[test]
    fn link_with_invalid_hreflangs() {
        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .link_with(|link| {
                link.href("href1")
                    .hreflang("it")
                    .hreflang("i18")
                    .hreflang("en")
            })
            .build()
            .unwrap_err();

        assert_eq!(error, Error::InvalidLanguageTag("i18".to_string()));
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
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
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
    fn auto_security() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.auto()
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
                security: vec!["auto".to_string()],
                security_definitions: [(
                    "auto".to_string(),
                    SecurityScheme {
                        attype: Some(vec!["ty1".to_string(), "ty2".to_string()]),
                        description: Some("desc".to_string()),
                        descriptions: Some(
                            [
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
                            ]
                            .into_iter()
                            .collect()
                        ),
                        proxy: Some("proxy".to_string()),
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Auto)
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
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
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
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
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
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
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
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
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
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
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
                                ("en".parse().unwrap(), "desc_en".to_string()),
                                ("it".parse().unwrap(), "desc_it".to_string()),
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
                    .additional_response(|b| b.content_type("application/xml").schema("schema1"))
                    .additional_response(|b| b.success().schema("schema2"))
                    .additional_response(|b| b)
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
                    additional_responses: Some(vec![
                        AdditionalExpectedResponse {
                            success: false,
                            content_type: Some("application/xml".to_string()),
                            schema: Some("schema1".to_string()),
                        },
                        AdditionalExpectedResponse {
                            success: true,
                            content_type: None,
                            schema: Some("schema2".to_string()),
                        },
                        AdditionalExpectedResponse::default(),
                    ]),
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

        assert_eq!(
            err,
            Error::InvalidOpInForm {
                context: FormContext::Thing,
                operation: FormOperation::ReadProperty
            }
        );
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
                                    default: None,
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
                                    default: None,
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
                                    default: None,
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
                                    default: None,
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
                                    minimum: Some(Minimum::Inclusive(5.)),
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
                    other: Nil::cons(FormExtA {
                        a: "test".to_string()
                    })
                    .cons(FormExtB { b: B(42) }),
                    content_type: Default::default(),
                    content_coding: Default::default(),
                    subprotocol: Default::default(),
                    security: Default::default(),
                    scopes: Default::default(),
                    response: Default::default(),
                    additional_responses: Default::default(),
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
                other: Nil::cons(FormExtA {
                    a: A("a".to_string())
                })
                .cons(FormExtB { c: B(1) }),
                response: Some(ExpectedResponse {
                    content_type: "application/json".to_string(),
                    other: Nil::cons(ExpectedResponseExtA {
                        b: A("b".to_string())
                    })
                    .cons(ExpectedResponseExtB { d: B(2) })
                }),
                content_type: Default::default(),
                content_coding: Default::default(),
                subprotocol: Default::default(),
                security: Default::default(),
                scopes: Default::default(),
                additional_responses: Default::default(),
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
                    .string()
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
                        .additional_response(|b| {
                            b.success().content_type("application/xml").schema("schema")
                        })
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
                            .string()
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
                other: Nil::cons(ThingA { a: 1, b: 2 })
                    .cons(ThingB {})
                    .cons(ThingC { c: 3 }),
                id: Some("id".to_string()),
                description: Some("description".to_string()),
                uri_variables: Some(
                    [(
                        "uri_variable".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::String(StringSchema::default())),
                            other: Nil::cons(DataSchemaExtA { h: 4 })
                                .cons(())
                                .cons(DataSchemaExtC { t: 5 }),
                            attype: Default::default(),
                            title: Default::default(),
                            titles: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            constant: Default::default(),
                            default: Default::default(),
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
                                other: Nil::cons(InteractionAffordanceExtA { d: 6 })
                                    .cons(InteractionAffordanceExtB { j: 9. })
                                    .cons(InteractionAffordanceExtC { p: 10 }),
                                attype: Default::default(),
                                title: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                forms: vec![Form {
                                    href: "href1".to_string(),
                                    response: Some(ExpectedResponse {
                                        content_type: "application/json".to_string(),
                                        other: Nil::cons(ExpectedResponseExtA { g: 16 })
                                            .cons(ExpectedResponseExtB { n: 17 })
                                            .cons(ExpectedResponseExtC { s: 18 })
                                    }),
                                    additional_responses: Some(vec![AdditionalExpectedResponse {
                                        success: true,
                                        content_type: Some("application/xml".to_string()),
                                        schema: Some("schema".to_string()),
                                    }]),
                                    other: Nil::cons(()).cons(FormExtB { m: 19 }).cons(()),
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
                                    other: Nil::cons(ObjectSchemaExtA { i: 11 })
                                        .cons(ObjectSchemaExtB { o: 12 })
                                        .cons(ObjectSchemaExtC { u: 13 }),
                                    properties: Default::default(),
                                    required: Default::default(),
                                })),
                                other: Nil::cons(DataSchemaExtA { h: 7 })
                                    .cons(())
                                    .cons(DataSchemaExtC { t: 8 }),
                                attype: Default::default(),
                                title: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                constant: Default::default(),
                                default: Default::default(),
                                unit: Default::default(),
                                one_of: Default::default(),
                                enumeration: Default::default(),
                                read_only: Default::default(),
                                write_only: Default::default(),
                                format: Default::default(),
                            },
                            other: Nil::cons(())
                                .cons(PropertyAffordanceExtB { k: 14. })
                                .cons(PropertyAffordanceExtC { q: 15 }),
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
                                            subtype: Some(DataSchemaSubtype::String(
                                                StringSchema::default()
                                            )),
                                            other: Nil::cons(DataSchemaExtA { h: 27 })
                                                .cons(())
                                                .cons(DataSchemaExtC { t: 28 }),
                                            attype: Default::default(),
                                            title: Default::default(),
                                            titles: Default::default(),
                                            description: Default::default(),
                                            descriptions: Default::default(),
                                            constant: Default::default(),
                                            default: Default::default(),
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
                                other: Nil::cons(InteractionAffordanceExtA { d: 22 })
                                    .cons(InteractionAffordanceExtB { j: 23. })
                                    .cons(InteractionAffordanceExtC { p: 24 }),
                                attype: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                forms: Default::default(),
                            },
                            input: Some(DataSchema {
                                title: Some("input".to_string()),
                                subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                                    minimum: Some(Minimum::Inclusive(0.)),
                                    maximum: Some(Maximum::Inclusive(5.)),
                                    ..Default::default()
                                })),
                                other: Nil::cons(DataSchemaExtA { h: 25 })
                                    .cons(())
                                    .cons(DataSchemaExtC { t: 26 }),
                                attype: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                constant: Default::default(),
                                default: Default::default(),
                                unit: Default::default(),
                                one_of: Default::default(),
                                enumeration: Default::default(),
                                read_only: Default::default(),
                                write_only: Default::default(),
                                format: Default::default(),
                            }),
                            other: Nil::cons(ActionAffordanceExtA { e: 20 })
                                .cons(())
                                .cons(ActionAffordanceExtC { r: 21 }),
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
                                other: Nil::cons(InteractionAffordanceExtA { d: 31 })
                                    .cons(InteractionAffordanceExtB { j: 32. })
                                    .cons(InteractionAffordanceExtC { p: 33 }),
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
                                other: Nil::cons(DataSchemaExtA { h: 34 })
                                    .cons(())
                                    .cons(DataSchemaExtC { t: 35 }),
                                attype: Default::default(),
                                title: Default::default(),
                                titles: Default::default(),
                                description: Default::default(),
                                descriptions: Default::default(),
                                constant: Default::default(),
                                default: Default::default(),
                                unit: Default::default(),
                                one_of: Default::default(),
                                enumeration: Default::default(),
                                read_only: Default::default(),
                                write_only: Default::default(),
                                format: Default::default(),
                            }),
                            other: Nil::cons(EventAffordanceExtA { f: 29 })
                                .cons(EventAffordanceExtB { l: 30 })
                                .cons(()),
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
                        other: Nil::cons(ExpectedResponseExtA { g: 37 })
                            .cons(ExpectedResponseExtB { n: 38 })
                            .cons(ExpectedResponseExtC { s: 39 })
                    }),
                    other: Nil::cons(()).cons(FormExtB { m: 36 }).cons(()),
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    content_type: Default::default(),
                    content_coding: Default::default(),
                    subprotocol: Default::default(),
                    security: Default::default(),
                    scopes: Default::default(),
                    additional_responses: Default::default(),
                }]),
                schema_definitions: Some(
                    [(
                        "schema".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::Null),
                            other: Nil::cons(DataSchemaExtA { h: 40 })
                                .cons(())
                                .cons(DataSchemaExtC { t: 41 }),
                            attype: Default::default(),
                            title: Default::default(),
                            titles: Default::default(),
                            description: Default::default(),
                            descriptions: Default::default(),
                            constant: Default::default(),
                            default: Default::default(),
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

    #[test]
    fn additional_response() {
        let mut builder = AdditionalExpectedResponseBuilder::new();
        assert_eq!(
            builder,
            AdditionalExpectedResponseBuilder {
                success: Default::default(),
                content_type: Default::default(),
                schema: Default::default(),
            },
        );

        assert_eq!(
            *builder.clone().success(),
            AdditionalExpectedResponseBuilder {
                success: true,
                content_type: Default::default(),
                schema: Default::default(),
            },
        );

        assert_eq!(
            *builder.clone().content_type("hello"),
            AdditionalExpectedResponseBuilder {
                success: Default::default(),
                content_type: Some("hello".to_string()),
                schema: Default::default(),
            },
        );

        assert_eq!(
            *builder.schema("schema"),
            AdditionalExpectedResponseBuilder {
                success: Default::default(),
                content_type: Default::default(),
                schema: Some("schema".to_string()),
            },
        );
    }

    #[test]
    fn additional_response_with_missing_schema() {
        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .schema_definition("schema1", |b| b.finish_extend().null())
            .schema_definition("schema2", |b| b.finish_extend().number().minimum(5.))
            .form(|b| {
                b.href("href")
                    .op(FormOperation::ReadAllProperties)
                    .additional_response(|b| b.schema("invalid_schema"))
            })
            .build()
            .unwrap_err();

        assert_eq!(
            error,
            Error::MissingSchemaDefinition("invalid_schema".to_string())
        );
    }

    #[test]
    fn invalid_thing_uri_variables() {
        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .uri_variable("uriVariable", |b| b.finish_extend().object())
            .build()
            .unwrap_err();

        assert_eq!(error, Error::InvalidUriVariables);

        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .uri_variable("uriVariable", |b| b.finish_extend().array())
            .build()
            .unwrap_err();

        assert_eq!(error, Error::InvalidUriVariables);
    }

    #[test]
    fn invalid_interaction_uri_variables() {
        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .action("action", |b| {
                b.uri_variable("uriVariable", |b| b.finish_extend().object())
            })
            .build()
            .unwrap_err();

        assert_eq!(error, Error::InvalidUriVariables);

        let error = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .property("property", |b| {
                b.finish_extend_data_schema()
                    .uri_variable("uriVariable", |b| b.finish_extend().array())
                    .string()
            })
            .build()
            .unwrap_err();

        assert_eq!(error, Error::InvalidUriVariables);
    }

    #[test]
    fn combo_security_scheme_with_all_of() {
        let builder = SecuritySchemeBuilder {
            attype: Default::default(),
            description: Default::default(),
            descriptions: Default::default(),
            proxy: Default::default(),
            name: Default::default(),
            subtype: (),
            required: Default::default(),
        }
        .combo()
        .attype("attype")
        .all_of(["schema1", "schema2"])
        .extend(["schema3", "schema4"])
        .push("schema1")
        .description("description");

        assert_eq!(builder.attype, Some(vec!["attype".to_string()]));
        assert_eq!(builder.description, Some("description".to_string()));
        assert_eq!(
            builder.subtype,
            (
                AllOfComboSecuritySchemeTag,
                ["schema1", "schema2", "schema3", "schema4", "schema1"]
                    .map(String::from)
                    .into(),
            ),
        );

        let subtype = builder.subtype.build();
        assert_eq!(
            subtype,
            SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Combo(
                ComboSecurityScheme::AllOf(
                    ["schema1", "schema2", "schema3", "schema4", "schema1"]
                        .map(String::from)
                        .into()
                )
            ))
        );
    }

    #[test]
    fn combo_security_scheme_with_one_of() {
        let builder = SecuritySchemeBuilder {
            attype: Default::default(),
            description: Default::default(),
            descriptions: Default::default(),
            proxy: Default::default(),
            name: Default::default(),
            subtype: (),
            required: Default::default(),
        }
        .combo()
        .attype("attype")
        .one_of(["schema1", "schema2"])
        .extend(["schema3", "schema4"])
        .push("schema1")
        .description("description");

        assert_eq!(builder.attype, Some(vec!["attype".to_string()]));
        assert_eq!(builder.description, Some("description".to_string()));
        assert_eq!(
            builder.subtype,
            (
                OneOfComboSecuritySchemeTag,
                ["schema1", "schema2", "schema3", "schema4", "schema1"]
                    .map(String::from)
                    .into(),
            ),
        );

        let subtype = builder.subtype.build();
        assert_eq!(
            subtype,
            SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Combo(
                ComboSecurityScheme::OneOf(
                    ["schema1", "schema2", "schema3", "schema4", "schema1"]
                        .map(String::from)
                        .into()
                )
            ))
        );
    }

    #[test]
    fn valid_combo_security_scheme() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| b.basic())
            .security(|b| b.combo().one_of(["basic", "nosec"]))
            .security(|b| b.no_sec())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT_11.into(),
                title: "MyLampThing".to_string(),
                security_definitions: [
                    (
                        "basic".to_string(),
                        SecurityScheme {
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Basic(Default::default())
                            ),
                            ..Default::default()
                        }
                    ),
                    (
                        "combo".to_string(),
                        SecurityScheme {
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::Combo(ComboSecurityScheme::OneOf(
                                    vec!["basic".to_string(), "nosec".to_string()]
                                ))
                            ),
                            ..Default::default()
                        }
                    ),
                    (
                        "nosec".to_string(),
                        SecurityScheme {
                            subtype: SecuritySchemeSubtype::Known(
                                KnownSecuritySchemeSubtype::NoSec
                            ),
                            ..Default::default()
                        }
                    ),
                ]
                .into_iter()
                .collect(),
                ..Default::default()
            },
        );
    }

    #[test]
    fn missing_combo_security_scheme() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| b.combo().one_of(["basic", "nosec"]))
            .security(|b| b.no_sec())
            .build()
            .unwrap_err();

        assert_eq!(err, Error::MissingSchemaDefinition("basic".to_string()));
    }

    #[test]
    fn checked_op_in_form() {
        let thing = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .form(|b| {
                b.op(FormOperation::ReadAllProperties)
                    .op(FormOperation::WriteAllProperties)
                    .op(FormOperation::ReadMultipleProperties)
                    .op(FormOperation::WriteMultipleProperties)
                    .op(FormOperation::ObserveAllProperties)
                    .op(FormOperation::UnobserveAllProperties)
                    .op(FormOperation::SubscribeAllEvents)
                    .op(FormOperation::UnsubscribeAllEvents)
                    .op(FormOperation::QueryAllActions)
                    .href("href")
            })
            .property("property", |b| {
                b.finish_extend_data_schema().null().form(|b| {
                    b.op(FormOperation::ReadProperty)
                        .op(FormOperation::WriteProperty)
                        .op(FormOperation::ObserveProperty)
                        .op(FormOperation::UnobserveProperty)
                        .href("href")
                })
            })
            .action("action", |b| {
                b.form(|b| {
                    b.op(FormOperation::InvokeAction)
                        .op(FormOperation::QueryAction)
                        .op(FormOperation::CancelAction)
                        .href("href")
                })
            })
            .event("event", |b| {
                b.form(|b| {
                    b.op(FormOperation::SubscribeEvent)
                        .op(FormOperation::UnsubscribeEvent)
                        .href("href")
                })
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
                        FormOperation::WriteAllProperties,
                        FormOperation::ReadMultipleProperties,
                        FormOperation::WriteMultipleProperties,
                        FormOperation::ObserveAllProperties,
                        FormOperation::UnobserveAllProperties,
                        FormOperation::SubscribeAllEvents,
                        FormOperation::UnsubscribeAllEvents,
                        FormOperation::QueryAllActions
                    ]),
                    href: "href".to_string(),
                    ..Default::default()
                }]),
                properties: Some(
                    [(
                        "property".to_string(),
                        PropertyAffordance {
                            interaction: InteractionAffordance {
                                forms: vec![Form {
                                    op: DefaultedFormOperations::Custom(vec![
                                        FormOperation::ReadProperty,
                                        FormOperation::WriteProperty,
                                        FormOperation::ObserveProperty,
                                        FormOperation::UnobserveProperty,
                                    ]),
                                    href: "href".to_string(),
                                    ..Default::default()
                                }],
                                ..Default::default()
                            },
                            data_schema: DataSchema {
                                subtype: Some(DataSchemaSubtype::Null),
                                ..Default::default()
                            },
                            ..Default::default()
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
                                forms: vec![Form {
                                    op: DefaultedFormOperations::Custom(vec![
                                        FormOperation::InvokeAction,
                                        FormOperation::QueryAction,
                                        FormOperation::CancelAction,
                                    ]),
                                    href: "href".to_string(),
                                    ..Default::default()
                                }],
                                ..Default::default()
                            },
                            ..Default::default()
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
                                forms: vec![Form {
                                    op: DefaultedFormOperations::Custom(vec![
                                        FormOperation::SubscribeEvent,
                                        FormOperation::UnsubscribeEvent,
                                    ]),
                                    href: "href".to_string(),
                                    ..Default::default()
                                }],
                                ..Default::default()
                            },
                            ..Default::default()
                        }
                    )]
                    .into_iter()
                    .collect()
                ),
                ..Default::default()
            },
        )
    }

    #[test]
    fn invalid_form_with_invalid_op_in_property_affordance() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .property("property", |b| {
                b.finish_extend_data_schema().null().form(|b| {
                    b.op(FormOperation::ReadProperty)
                        .op(FormOperation::ReadAllProperties)
                        .op(FormOperation::WriteAllProperties)
                        .href("href")
                })
            })
            .build()
            .unwrap_err();

        assert_eq!(
            err,
            Error::InvalidOpInForm {
                context: FormContext::Property,
                operation: FormOperation::ReadAllProperties
            }
        );
    }

    #[test]
    fn invalid_form_with_invalid_op_in_action_affordance() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .action("action", |b| {
                b.form(|b| {
                    b.op(FormOperation::InvokeAction)
                        .op(FormOperation::WriteProperty)
                        .op(FormOperation::QueryAction)
                        .href("href")
                })
            })
            .build()
            .unwrap_err();

        assert_eq!(
            err,
            Error::InvalidOpInForm {
                context: FormContext::Action,
                operation: FormOperation::WriteProperty
            }
        );
    }

    #[test]
    fn invalid_form_with_invalid_op_in_event_affordance() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .finish_extend()
            .event("event", |b| {
                b.form(|b| {
                    b.op(FormOperation::SubscribeEvent)
                        .op(FormOperation::ReadProperty)
                        .op(FormOperation::UnsubscribeEvent)
                        .href("href")
                })
            })
            .build()
            .unwrap_err();

        assert_eq!(
            err,
            Error::InvalidOpInForm {
                context: FormContext::Event,
                operation: FormOperation::ReadProperty
            }
        );
    }

    #[test]
    fn form_operation_serialize_display_coherence() {
        const OPS: [FormOperation; 18] = [
            FormOperation::ReadProperty,
            FormOperation::WriteProperty,
            FormOperation::ObserveProperty,
            FormOperation::UnobserveProperty,
            FormOperation::InvokeAction,
            FormOperation::QueryAction,
            FormOperation::CancelAction,
            FormOperation::SubscribeEvent,
            FormOperation::UnsubscribeEvent,
            FormOperation::ReadAllProperties,
            FormOperation::WriteAllProperties,
            FormOperation::ReadMultipleProperties,
            FormOperation::WriteMultipleProperties,
            FormOperation::ObserveAllProperties,
            FormOperation::UnobserveAllProperties,
            FormOperation::SubscribeAllEvents,
            FormOperation::UnsubscribeAllEvents,
            FormOperation::QueryAllActions,
        ];

        for op in OPS {
            assert_eq!(
                serde_json::to_value(&op).unwrap(),
                Value::String(op.to_string())
            );
        }
    }

    #[test]
    fn convert_valid_unchecked_security_schema() {
        let schema = UncheckedSecurityScheme {
            attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
            description: Some("description".to_string()),
            descriptions: Some({
                let mut multilang = MultiLanguageBuilder::default();
                multilang
                    .add("it", "description1")
                    .add("en", "description2");
                multilang
            }),
            proxy: Some("proxy".to_string()),
            subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Psk(
                PskSecurityScheme {
                    identity: Some("identity".to_string()),
                },
            )),
        };

        assert_eq!(
            SecurityScheme::try_from(schema).unwrap(),
            SecurityScheme {
                attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
                description: Some("description".to_string()),
                descriptions: Some(
                    [
                        ("it".parse().unwrap(), "description1".to_string()),
                        ("en".parse().unwrap(), "description2".to_string())
                    ]
                    .into_iter()
                    .collect(),
                ),
                proxy: Some("proxy".to_string()),
                subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Psk(
                    PskSecurityScheme {
                        identity: Some("identity".to_string()),
                    },
                )),
            },
        );
    }

    #[test]
    fn convert_invalid_unchecked_security_schema() {
        let schema = UncheckedSecurityScheme {
            attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
            description: Some("description".to_string()),
            descriptions: Some({
                let mut multilang = MultiLanguageBuilder::default();
                multilang
                    .add("it", "description1")
                    .add("e1n", "description2");
                multilang
            }),
            proxy: Some("proxy".to_string()),
            subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Psk(
                PskSecurityScheme {
                    identity: Some("identity".to_string()),
                },
            )),
        };

        assert_eq!(
            SecurityScheme::try_from(schema).unwrap_err(),
            Error::InvalidLanguageTag("e1n".to_string()),
        );
    }

    #[test]
    fn invalid_language_tag() {
        let err = ThingBuilder::<Nil, _>::new("MyLampThing")
            .security(|b| {
                b.auto()
                    .descriptions(|ml| ml.add("en", "desc_en").add("i1t", "desc_it"))
            })
            .build()
            .unwrap_err();
        assert_eq!(err, Error::InvalidLanguageTag("i1t".to_string()));
    }
}
