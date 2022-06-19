pub mod affordance;
pub mod data_schema;
pub mod human_readable_info;

use std::{borrow::Cow, collections::HashMap, fmt, ops::Not};

use serde_json::Value;
use time::OffsetDateTime;

use crate::thing::{
    ApiKeySecurityScheme, BasicSecurityScheme, BearerSecurityScheme, DataSchema,
    DefaultedFormOperations, DigestSecurityScheme, ExpectedResponse, Form, FormOperation,
    KnownSecuritySchemeSubtype, Link, MultiLanguage, OAuth2SecurityScheme, PskSecurityScheme,
    QualityOfProtection, SecurityAuthenticationLocation, SecurityScheme, SecuritySchemeSubtype,
    Thing, UnknownSecuritySchemeSubtype, VersionInfo, TD_CONTEXT,
};

use self::{
    affordance::{
        ActionAffordanceBuilder, AffordanceBuilder, CheckableInteractionAffordanceBuilder,
        EventAffordanceBuilder, PropertyAffordanceBuilder, UsableActionAffordanceBuilder,
        UsableEventAffordanceBuilder, UsablePropertyAffordanceBuilder,
    },
    data_schema::{CheckableDataSchema, PartialDataSchemaBuilder},
};

/// Builder for WoT Thing
///
/// TODO: Write an example usage
#[must_use]
pub struct ThingBuilder {
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
    properties: Vec<AffordanceBuilder<UsablePropertyAffordanceBuilder>>,
    actions: Vec<AffordanceBuilder<UsableActionAffordanceBuilder>>,
    events: Vec<AffordanceBuilder<UsableEventAffordanceBuilder>>,
    links: Option<Vec<Link>>,
    forms: Option<Vec<FormBuilder<String>>>,
    security: Vec<String>,
    security_definitions: Vec<(String, SecurityScheme)>,
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

impl ThingBuilder {
    /// Create a new default builder with a specified title
    pub fn new(title: impl Into<String>) -> Self {
        let title = title.into();
        let context = vec![Context::Simple(TD_CONTEXT.to_string())];

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
        }
    }

    /// Consume the builder to produce the configured Thing
    ///
    /// This step will perform the final validation of the builder state.
    pub fn build(self) -> Result<Thing, Error> {
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
        })
    }

    fn build_form_from_builder(
        form_builder: FormBuilder<String>,
        security_definitions: &HashMap<String, SecurityScheme>,
    ) -> Result<Form, Error> {
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

        let content_type = content_type.unwrap_or(Form::default_content_type());
        Ok(Form {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
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
        if value.as_ref() == TD_CONTEXT {
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

    /// Add a Thing-level form
    ///
    /// NOTE:
    ///     - It must explicitly state its operation
    ///     - It must use an `all` operation
    pub fn form<F>(mut self, f: F) -> Self
    where
        F: FnOnce(FormBuilder<()>) -> FormBuilder<String>,
    {
        self.forms
            .get_or_insert_with(Default::default)
            .push(f(FormBuilder::new()));
        self
    }

    pub fn property<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(PropertyAffordanceBuilder<PartialDataSchemaBuilder>) -> T,
        T: Into<PropertyAffordanceBuilder<DataSchema>>,
    {
        let affordance = f(PropertyAffordanceBuilder::default()).into();
        let affordance_builder = AffordanceBuilder {
            name: name.into(),
            affordance,
        };
        self.properties.push(affordance_builder);
        self
    }

    pub fn action<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(ActionAffordanceBuilder<(), ()>) -> T,
        T: Into<ActionAffordanceBuilder<Option<DataSchema>, Option<DataSchema>>>,
    {
        let affordance = f(ActionAffordanceBuilder::default()).into();
        let affordance_builder = AffordanceBuilder {
            name: name.into(),
            affordance,
        };
        self.actions.push(affordance_builder);
        self
    }

    pub fn event<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(EventAffordanceBuilder<(), (), ()>) -> T,
        T: Into<EventAffordanceBuilder<Option<DataSchema>, Option<DataSchema>, Option<DataSchema>>>,
    {
        let affordance = f(EventAffordanceBuilder::default()).into();
        let affordance_builder = AffordanceBuilder {
            name: name.into(),
            affordance,
        };
        self.events.push(affordance_builder);
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

pub struct SecuritySchemeNoSecTag;

pub trait BuildableSecuritySchemeSubtype {
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

pub trait HasNameLocation {
    fn location_mut(&mut self) -> &mut SecurityAuthenticationLocation;
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
pub struct FormBuilder<Href> {
    op: DefaultedFormOperations,
    href: Href,
    content_type: Option<Cow<'static, str>>,
    content_coding: Option<String>,
    subprotocol: Option<String>,
    security: Option<Vec<String>>,
    scopes: Option<Vec<String>>,
    response: Option<ExpectedResponse>,
}

impl FormBuilder<()> {
    fn new() -> Self {
        Self {
            op: Default::default(),
            href: (),
            content_type: Default::default(),
            content_coding: Default::default(),
            subprotocol: Default::default(),
            security: Default::default(),
            scopes: Default::default(),
            response: Default::default(),
        }
    }

    /// Create a new builder with the specified Href
    pub fn href(self, value: impl Into<String>) -> FormBuilder<String> {
        let Self {
            op,
            href: (),
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
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
        }
    }
}

impl<T> FormBuilder<T> {
    opt_field_builder!(
        content_type: Cow<'static, str>,
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

    /// Set the expected response metadata
    ///
    /// It is optional if the input and output metadata are the same, e.g. the content_type
    /// matches.
    pub fn response(
        mut self,
        content_type: impl Into<String>,
        other_fields: impl Into<Value>,
    ) -> Self {
        self.response = Some(ExpectedResponse {
            content_type: content_type.into(),
            other: other_fields.into(),
        });
        self
    }
}

impl From<FormBuilder<String>> for Form {
    fn from(builder: FormBuilder<String>) -> Self {
        let FormBuilder {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
        } = builder;

        let content_type = content_type.unwrap_or(Form::default_content_type());

        Self {
            op,
            href,
            content_type,
            content_coding,
            subprotocol,
            security,
            scopes,
            response,
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;
    use time::macros::datetime;

    use crate::{
        builder::{
            affordance::BuildableInteractionAffordance, data_schema::SpecializableDataSchema,
            human_readable_info::BuildableHumanReadableInfo,
        },
        thing::{
            ActionAffordance, DataSchemaSubtype, EventAffordance, InteractionAffordance,
            PropertyAffordance,
        },
    };

    use super::*;

    macro_rules! test_opt_string_field_builder {
        ($($field:ident),* $(,)?) => {
            $(
                #[test]
                pub fn $field() {
                    let thing = ThingBuilder::new("MyLampThing").$field("test").build().unwrap();

                    assert_eq!(
                        thing,
                        Thing {
                            context: TD_CONTEXT.into(),
                            title: "MyLampThing".to_string(),
                            $field: Some("test".into()),
                            ..Thing::empty()
                        }
                    );
                }
            )*
        };
    }

    #[test]
    fn default_context() {
        let thing = ThingBuilder::new("MyLampThing").build().unwrap();
        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                ..Thing::empty()
            }
        )
    }

    #[test]
    fn redundant_default_context() {
        let thing = ThingBuilder::new("MyLampThing")
            .context(TD_CONTEXT)
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                ..Thing::empty()
            }
        )
    }

    #[test]
    fn simple_contexts() {
        let thing = ThingBuilder::new("MyLampThing")
            .context("test")
            .context("another_test")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: json! {[
                    TD_CONTEXT,
                    "test",
                    "another_test",
                ]},
                title: "MyLampThing".to_string(),
                ..Thing::empty()
            }
        )
    }

    #[test]
    fn map_contexts() {
        let thing = ThingBuilder::new("MyLampThing")
            .context_map(|b| b.context("hello", "world").context("all", "fine"))
            .context("simple")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: json! {[
                    TD_CONTEXT,
                    {
                        "hello": "world",
                        "all": "fine",
                    },
                    "simple",
                ]},
                title: "MyLampThing".to_string(),
                ..Thing::empty()
            }
        )
    }

    test_opt_string_field_builder!(id, description, version, support, base);

    #[test]
    fn attype() {
        let thing = ThingBuilder::new("MyLampThing")
            .attype("test")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                attype: Some(vec!["test".to_string()]),
                ..Thing::empty()
            }
        );

        let thing = ThingBuilder::new("MyLampThing")
            .attype("test1")
            .attype("test2")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                attype: Some(vec!["test1".to_string(), "test2".to_string()]),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn titles() {
        let thing = ThingBuilder::new("MyLampThing")
            .titles(|ml| ml.add("en", "My lamp").add("it", "La mia lampada"))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                titles: Some(
                    [("en", "My lamp"), ("it", "La mia lampada")]
                        .into_iter()
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .collect()
                ),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn descriptions() {
        let thing = ThingBuilder::new("MyLampThing")
            .description("My Lamp")
            .descriptions(|ml| ml.add("en", "My lamp").add("it", "La mia lampada"))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                description: Some("My Lamp".to_string()),
                descriptions: Some(
                    [("en", "My lamp"), ("it", "La mia lampada")]
                        .into_iter()
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .collect()
                ),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn created() {
        const DATETIME: OffsetDateTime = datetime!(2022-05-01 12:13:14.567 +01:00);
        let thing = ThingBuilder::new("MyLampThing")
            .created(DATETIME)
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                created: Some(DATETIME),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn modified() {
        const DATETIME: OffsetDateTime = datetime!(2022-05-01 12:13:14.567 +01:00);
        let thing = ThingBuilder::new("MyLampThing")
            .modified(DATETIME)
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                modified: Some(DATETIME),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn link_simple() {
        let thing = ThingBuilder::new("MyLampThing")
            .link("href1")
            .link("href2")
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn link_with() {
        let thing = ThingBuilder::new("MyLampThing")
            .link_with(|link| link.href("href1").ty("ty").rel("rel").anchor("anchor"))
            .link_with(|link| link.href("href2"))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn nosec_security() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn basic_security() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn digest_security() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn apikey_security() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn bearer_security() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn oauth2_security() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn custom_security() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn named_security() {
        let thing = ThingBuilder::new("MyLampThing")
            .security(|b| b.no_sec().with_key("test_sec1").required())
            .security(|b| b.no_sec().with_key("test_sec2").required())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn mixed_security() {
        let thing = ThingBuilder::new("MyLampThing")
            .security(|b| b.digest().with_key("sec1"))
            .security(|b| b.basic().with_key("sec2").required())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );

        let thing = ThingBuilder::new("MyLampThing")
            .security(|b| b.digest())
            .security(|b| b.basic().required())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn colliding_security_names() {
        let err = ThingBuilder::new("MyLampThing")
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
        let thing = ThingBuilder::new("MyLampThing")
            .form(|form| form.href("href").op(FormOperation::ReadAllProperties))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    href: "href".to_string(),
                    content_type: Form::default_content_type(),
                    content_coding: Default::default(),
                    subprotocol: Default::default(),
                    security: Default::default(),
                    scopes: Default::default(),
                    response: Default::default(),
                }]),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn complete_form() {
        let thing = ThingBuilder::new("MyLampThing")
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
                    .response(
                        "application/json",
                        json!({
                            "response": {
                                "test1": 1,
                                "test2": "2",
                            },
                        }),
                    )
            })
            .security(|b| b.digest())
            .security(|b| b.basic())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![FormOperation::ReadAllProperties]),
                    href: "href".to_string(),
                    content_type: "text/plain".into(),
                    content_coding: Some("coding".to_string()),
                    subprotocol: Some("subprotocol".to_string()),
                    security: Some(vec!["digest".to_string(), "basic".to_string()]),
                    scopes: Some(vec!["scope1".to_string(), "scope2".to_string()]),
                    response: Some(ExpectedResponse {
                        content_type: "application/json".to_string(),
                        other: json!({
                            "response": {
                                "test1": 1,
                                "test2": "2",
                            },
                        })
                    }),
                }]),
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
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn form_with_multiple_ops() {
        let thing = ThingBuilder::new("MyLampThing")
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
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                forms: Some(vec![Form {
                    op: DefaultedFormOperations::Custom(vec![
                        FormOperation::ReadAllProperties,
                        FormOperation::ReadMultipleProperties
                    ]),
                    href: "href".to_string(),
                    content_type: Form::default_content_type(),
                    content_coding: Default::default(),
                    subprotocol: Default::default(),
                    security: Default::default(),
                    scopes: Default::default(),
                    response: Default::default(),
                }]),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn invalid_form_without_op() {
        let err = ThingBuilder::new("MyLampThing")
            .form(|form| form.href("href"))
            .build()
            .unwrap_err();

        assert_eq!(err, Error::MissingOpInForm);
    }

    #[test]
    fn invalid_form_with_invalid_op() {
        let err = ThingBuilder::new("MyLampThing")
            .form(|form| form.href("href").op(FormOperation::ReadProperty))
            .build()
            .unwrap_err();

        assert_eq!(err, Error::InvalidOpInForm);
    }

    #[test]
    fn invalid_form_with_missing_security() {
        let err = ThingBuilder::new("MyLampThing")
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
        let thing = ThingBuilder::new("MyLampThing")
            .property("on", |b| b.bool().observable(true).title("title"))
            .property("prop", |b| b.null())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                                    subtype: Some(DataSchemaSubtype::Boolean)
                                },
                                observable: Some(true),
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
                                    subtype: Some(DataSchemaSubtype::Null)
                                },
                                observable: None,
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn with_action_affordance() {
        let thing = ThingBuilder::new("MyLampThing")
            .action("fade", |b| b)
            .action("action", |b| {
                b.title("title").idempotent().input(|b| b.null())
            })
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                                },
                                input: None,
                                output: None,
                                safe: false,
                                idempotent: false,
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
                                    subtype: Some(DataSchemaSubtype::Null)
                                }),
                                output: None,
                                safe: false,
                                idempotent: true,
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn with_event_affordance() {
        let thing = ThingBuilder::new("MyLampThing")
            .event("overheat", |b| b)
            .event("event", |b| b.title("title").cancellation(|b| b.null()))
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
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
                                },
                                subscription: None,
                                data: None,
                                cancellation: None,
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
                                    subtype: Some(DataSchemaSubtype::Null)
                                }),
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn valid_affordance_security() {
        let thing = ThingBuilder::new("MyLampThing")
            .property("on", |b| {
                b.bool().form(|b| b.security("basic").href("href"))
            })
            .security(|b| b.basic())
            .build()
            .unwrap();

        assert_eq!(
            thing,
            Thing {
                context: TD_CONTEXT.into(),
                title: "MyLampThing".to_string(),
                properties: Some(
                    [(
                        "on".to_owned(),
                        PropertyAffordance {
                            interaction: InteractionAffordance {
                                attype: None,
                                title: None,
                                titles: None,
                                description: None,
                                descriptions: None,
                                forms: vec![Form {
                                    op: DefaultedFormOperations::Default,
                                    href: "href".to_owned(),
                                    content_type: Form::default_content_type(),
                                    content_coding: None,
                                    subprotocol: None,
                                    security: Some(vec!["basic".to_owned()]),
                                    scopes: None,
                                    response: None
                                }],
                                uri_variables: None,
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
                                subtype: Some(DataSchemaSubtype::Boolean)
                            },
                            observable: None,
                        }
                    ),]
                    .into_iter()
                    .collect()
                ),
                security_definitions: [(
                    "basic".to_owned(),
                    SecurityScheme {
                        attype: None,
                        description: None,
                        descriptions: None,
                        proxy: None,
                        subtype: SecuritySchemeSubtype::Known(KnownSecuritySchemeSubtype::Basic(
                            BasicSecurityScheme {
                                location: SecurityAuthenticationLocation::Header,
                                name: None,
                            }
                        ))
                    }
                )]
                .into_iter()
                .collect(),
                ..Thing::empty()
            }
        );
    }

    #[test]
    fn invalid_affordance_security() {
        let error = ThingBuilder::new("MyLampThing")
            .property("on", |b| {
                b.bool().form(|b| b.security("oauth2").href("href"))
            })
            .security(|b| b.basic())
            .build()
            .unwrap_err();

        assert_eq!(error, Error::UndefinedSecurity("oauth2".to_owned()));
    }
}
