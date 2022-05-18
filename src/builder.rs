use std::{borrow::Cow, collections::HashMap};

use serde_json::Value;
use time::OffsetDateTime;

use crate::thing::{
    ActionAffordance, ApiKeySecurityScheme, BasicSecurityScheme, BearerSecurityScheme,
    DigestSecurityScheme, EventAffordance, Form, KnownSecuritySchemeSubtype, Link, MultiLanguage,
    OAuth2SecurityScheme, PropertyAffordance, PskSecurityScheme, QualityOfProtection,
    SecurityAuthenticationLocation, SecurityScheme, SecuritySchemeSubtype, Thing,
    UnknownSecuritySchemeSubtype, VersionInfo, TD_CONTEXT,
};

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
    properties: Option<HashMap<String, PropertyAffordance>>,
    actions: Option<HashMap<String, ActionAffordance>>,
    events: Option<HashMap<String, EventAffordance>>,
    links: Option<Vec<Link>>,
    forms: Option<Vec<Form>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
pub enum Error {
    #[error("Two security definitions use the name \"{0}\"")]
    DuplicatedSecurityDefinition(String),
}

impl ThingBuilder {
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

    opt_field_builder!(
        id: String,
        description: String,
        version: VersionInfo,
        created: OffsetDateTime,
        modified: OffsetDateTime,
        support: String,
        base: String,
    );

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

    pub fn context_map<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut ContextMapBuilder) -> &mut ContextMapBuilder,
    {
        let mut context_map = ContextMapBuilder(Default::default());
        f(&mut context_map);

        self.context.push(Context::Map(context_map.0));
        self
    }

    pub fn attype(mut self, value: impl Into<String>) -> Self {
        self.attype
            .get_or_insert_with(Default::default)
            .push(value.into());
        self
    }

    pub fn titles<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);

        self.titles = Some(builder.values);
        self
    }

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

#[must_use]
pub struct ContextMapBuilder(HashMap<String, String>);

impl ContextMapBuilder {
    pub fn context(&mut self, name: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.0.insert(name.into(), value.into());
        self
    }
}

#[derive(Default)]
pub struct MultiLanguageBuilder<T> {
    values: HashMap<String, T>,
}

impl<T> MultiLanguageBuilder<T> {
    pub fn add(&mut self, language: impl Into<String>, value: impl Into<T>) -> &mut Self {
        self.values.insert(language.into(), value.into());
        self
    }
}

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

    pub fn attype(mut self, ty: impl Into<String>) -> Self {
        self.attype
            .get_or_insert_with(Default::default)
            .push(ty.into());
        self
    }

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
    pub fn name(mut self, value: impl Into<String>) -> Self {
        *self.subtype.name_mut() = Some(value.into());
        self
    }

    pub fn location(mut self, value: SecurityAuthenticationLocation) -> Self {
        *self.subtype.location_mut() = value;
        self
    }
}

impl SecuritySchemeBuilder<DigestSecurityScheme> {
    pub fn qop(mut self, value: QualityOfProtection) -> Self {
        self.subtype.qop = value;
        self
    }
}

impl SecuritySchemeBuilder<BearerSecurityScheme> {
    pub fn authorization(mut self, value: impl Into<String>) -> Self {
        self.subtype.authorization = Some(value.into());
        self
    }

    pub fn alg(mut self, value: impl Into<Cow<'static, str>>) -> Self {
        self.subtype.alg = value.into();
        self
    }

    pub fn format(mut self, value: impl Into<Cow<'static, str>>) -> Self {
        self.subtype.format = value.into();
        self
    }
}

impl SecuritySchemeBuilder<OAuth2SecurityScheme> {
    pub fn authorization(mut self, value: impl Into<String>) -> Self {
        self.subtype.authorization = Some(value.into());
        self
    }

    pub fn token(mut self, value: impl Into<String>) -> Self {
        self.subtype.token = Some(value.into());
        self
    }

    pub fn refresh(mut self, value: impl Into<String>) -> Self {
        self.subtype.refresh = Some(value.into());
        self
    }

    pub fn scope(mut self, value: impl Into<String>) -> Self {
        self.subtype
            .scopes
            .get_or_insert_with(Default::default)
            .push(value.into());
        self
    }
}

impl SecuritySchemeBuilder<UnknownSecuritySchemeSubtype> {
    pub fn data(mut self, value: impl Into<Value>) -> Self {
        self.subtype.data = value.into();
        self
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;
    use time::macros::datetime;

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
}
