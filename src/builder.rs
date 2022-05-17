use std::collections::HashMap;

use serde_json::Value;
use time::OffsetDateTime;

use crate::thing::{
    ActionAffordance, EventAffordance, Form, Link, MultiLanguage, PropertyAffordance,
    SecurityScheme, Thing, VersionInfo, TD_CONTEXT,
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
    security_definitions: HashMap<String, SecurityScheme>,
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

    pub fn build(self) -> Thing {
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
            security_definitions,
        } = self;

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

        Thing {
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
        }
    }

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

    pub fn context_map(self) -> ContextMapBuilder {
        ContextMapBuilder {
            builder: self,
            map: Default::default(),
        }
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
pub struct ContextMapBuilder {
    builder: ThingBuilder,
    map: HashMap<String, String>,
}

impl ContextMapBuilder {
    pub fn context(mut self, name: impl Into<String>, value: impl Into<String>) -> Self {
        self.map.insert(name.into(), value.into());
        self
    }

    pub fn finish_context_map(mut self) -> ThingBuilder {
        self.builder.context.push(Context::Map(self.map));
        self.builder
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn default_context() {
        let thing = ThingBuilder::new("MyLampThing").build();
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
        let thing = ThingBuilder::new("MyLampThing").context(TD_CONTEXT).build();

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
            .build();

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
            .context_map()
            .context("hello", "world")
            .context("all", "fine")
            .finish_context_map()
            .context("simple")
            .build();

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
}
