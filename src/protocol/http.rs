use serde::{Deserialize, Serialize};
// use serde_with::with_prefix;

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum Method {
    Get,
    Put,
    Post,
    Delete,
    Patch,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct MessageHeader {
    #[serde(rename = "htv:fieldName")]
    field_name: Option<String>,
    #[serde(rename = "htv:fieldValue")]
    field_value: Option<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
struct Response {
    #[serde(rename = "htv:headers")]
    headers: Vec<MessageHeader>,
    #[serde(rename = "htv:statusCodeValue")]
    status_code_value: Option<usize>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
// #[serde(rename_all = "camelCase")]
struct Form {
    #[serde(rename = "htv:methodName")]
    method_name: Option<Method>,
}

// TODO: figure out what's wrong with with_prefix

pub(crate) mod mini {
    use crate::hlist::{Cons, Nil};
    use std::{borrow::Cow, collections::HashMap};

    use crate::thing::DefaultedFormOperations;
    use serde::{Deserialize, Serialize};
    use serde_json::Value;
    use serde_with::*;

    #[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct ExpectedResponse<T: Default = HashMap<String, Value>> {
        pub content_type: String,

        #[serde(flatten)]
        pub other: T,
    }

    impl<T: Default> ExpectedResponse<Cons<T, Nil>> {
        pub fn extend<U: Default>(self, e: U) -> ExpectedResponse<Cons<Cons<T, U>, Nil>> {
            let Self {
                content_type,
                other,
            } = self;

            let other = other.add(e);

            ExpectedResponse {
                content_type,
                other,
            }
        }
    }

    #[serde_as]
    #[skip_serializing_none]
    #[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Form<T: Default = HashMap<String, Value>, E: Default = HashMap<String, Value>> {
        #[serde(default)]
        pub op: DefaultedFormOperations,

        // FIXME: use AnyURI
        pub href: String,

        #[serde(default = "Form::<()>::default_content_type")]
        pub content_type: Cow<'static, str>,

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

        pub response: Option<ExpectedResponse<E>>,

        #[serde(flatten)]
        pub other: T,
    }

    impl<T: Default> Form<T> {
        pub(crate) const fn default_content_type() -> Cow<'static, str> {
            Cow::Borrowed("application/json")
        }
    }

    impl<T: Default, E: Default> Form<Cons<T, Nil>, Cons<E, Nil>> {
        pub fn extend<U: Default, F: Default>(
            self,
            u: U,
            f: F,
        ) -> Form<Cons<Cons<T, U>, Nil>, Cons<Cons<E, F>, Nil>> {
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
            } = self;

            let other = other.add(u);
            let response = response.map(|e| e.extend(f));

            Form {
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
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hlist::{Cons, Nil};

    fn deserialize_form(s: &str) {
        let f: super::Form = serde_json::from_str(s).unwrap();

        dbg!(&f);

        let f: mini::Form = serde_json::from_str(s).unwrap();

        dbg!(&f);

        let f: mini::Form<super::Form> = serde_json::from_str(s).unwrap();

        dbg!(&f);

        let f: mini::Form<Cons<super::Form, Nil>> = serde_json::from_str(s).unwrap();

        dbg!(&f);

        let f: mini::Form<super::Form, super::Response> = serde_json::from_str(s).unwrap();

        dbg!(&f);
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
            },
            "additionalResponses": [
                {
                    "description": "Invalid query arguments",
                    "contentType": "application/problem+json",
                    "htv:statusCodeValue": 400
                }
            ]
        }
        "#;

        deserialize_form(property);
    }
    /*
        #[test]
        fn deserialize_discovery_action() {
            let action = r#"
            {
                "href": "/things",
                "htv:methodName": "POST",
                "contentType": "application/td+json",
                "response": {
                    "description": "Success response including the system-generated URI",
                    "htv:headers": [
                        {
                            "description": "System-generated URI",
                            "htv:fieldName": "Location"
                        }
                    ],
                    "htv:statusCodeValue": 201
                },
                "additionalResponses": [
                    {
                        "description": "Invalid serialization or TD",
                        "contentType": "application/problem+json",
                        "htv:statusCodeValue": 400
                    }
                ]
            }
            "#;

            deserialize_form(action);
        }
    */
}
