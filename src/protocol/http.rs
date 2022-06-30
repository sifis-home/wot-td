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
    pub struct AdditionalExpectedResponse<T: Default + Clone = HashMap<String, Value>> {
        #[serde(default)]
        pub success: bool,
        pub content_type: String,

        #[serde(flatten)]
        pub other: T,
    }

    impl<T: Default + Clone> AdditionalExpectedResponse<Cons<T, Nil>> {
        pub fn extend<U: Default + Clone>(
            self,
            e: U,
        ) -> AdditionalExpectedResponse<Cons<Cons<T, U>, Nil>> {
            let Self {
                success,
                content_type,
                other,
            } = self;

            let other = other.add(e);

            AdditionalExpectedResponse {
                success,
                content_type,
                other,
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct ExpectedResponse<T: Default + Clone = HashMap<String, Value>> {
        pub content_type: String,

        #[serde(flatten)]
        pub other: T,
    }

    impl<T: Default + Clone> ExpectedResponse<Cons<T, Nil>> {
        pub fn extend<U: Default + Clone>(self, e: U) -> ExpectedResponse<Cons<Cons<T, U>, Nil>> {
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
    pub struct Form<
        T: Default + Clone = HashMap<String, Value>,
        E: Default + Clone = HashMap<String, Value>,
    > {
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

        pub additional_response: Option<AdditionalExpectedResponse<E>>,

        #[serde(flatten)]
        pub other: T,
    }

    impl<T: Default + Clone> Form<T> {
        pub(crate) const fn default_content_type() -> Cow<'static, str> {
            Cow::Borrowed("application/json")
        }
    }

    impl<T: Default + Clone, E: Default + Clone> Form<Cons<T, Nil>, Cons<E, Nil>> {
        pub fn extend<U: Default + Clone, F: Default + Clone>(
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
                additional_response,
                other,
            } = self;

            let other = other.add(u);
            let response = response.map(|e| e.extend(f.clone()));
            let additional_response = additional_response.map(|e| e.extend(f));

            Form {
                op,
                href,
                content_type,
                content_coding,
                subprotocol,
                security,
                scopes,
                response,
                additional_response,
                other,
            }
        }
    }

    #[serde_as]
    #[skip_serializing_none]
    #[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct InteractionAffordance<T, F, R>
    where
        T: Default + Clone,
        F: Default + Clone,
        R: Default + Clone,
    {
        #[serde(rename = "@type", default)]
        #[serde_as(as = "Option<OneOrMany<_>>")]
        pub attype: Option<Vec<String>>,

        pub title: Option<String>,

        pub description: Option<String>,

        pub forms: Vec<Form<F, R>>,

        #[serde(flatten)]
        pub other: T,
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
