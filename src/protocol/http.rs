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

use mini::{Buildable, Builder};

#[derive(Default)]
struct ResponseBuilder {
    headers: Vec<MessageHeader>,
    status_code_value: Option<usize>,
}

impl ResponseBuilder {
    pub fn headers(mut self, header: MessageHeader) -> Self {
        self.headers.push(header);
        self
    }
    pub fn status_code_value(mut self, value: usize) -> Self {
        self.status_code_value = Some(value);
        self
    }
}

impl Builder for ResponseBuilder {
    type B = Response;

    fn build(self) -> Response {
        let ResponseBuilder {
            headers,
            status_code_value,
        } = self;
        Response {
            headers,
            status_code_value,
        }
    }
}

impl Buildable for Response {
    type B = ResponseBuilder;

    fn builder() -> ResponseBuilder {
        ResponseBuilder::default()
    }
}

#[derive(Default)]
struct FormBuilder {
    method_name: Option<Method>,
}

impl FormBuilder {
    pub fn method(mut self, method_name: Method) -> Self {
        self.method_name = Some(method_name);

        self
    }
}

impl Builder for FormBuilder {
    type B = Form;

    fn build(self) -> Form {
        let FormBuilder { method_name } = self;

        Form { method_name }
    }
}

impl Buildable for Form {
    type B = FormBuilder;

    fn builder() -> FormBuilder {
        FormBuilder::default()
    }
}

// TODO: figure out what's wrong with with_prefix

pub(crate) mod mini {
    use crate::hlist::{Cons, Nil};
    use crate::thing::DefaultedFormOperations;
    use serde::{Deserialize, Serialize};
    use serde_with::*;
    use std::borrow::Cow;

    pub trait Builder: Default {
        type B: Buildable;

        fn build(self) -> Self::B;
    }

    pub trait Buildable: Default {
        type B: Builder;

        fn builder() -> Self::B;
    }

    impl Builder for Nil {
        type B = Nil;

        fn build(self) -> Nil {
            Nil
        }
    }

    impl Buildable for Nil {
        type B = Nil;

        fn builder() -> Nil {
            Nil
        }
    }

    impl<T: Builder, U: Builder> Builder for Cons<T, U> {
        type B = Cons<T::B, U::B>;

        fn build(self) -> Self::B {
            let Cons { head, tail } = self;
            let head = head.build();
            let tail = tail.build();

            Cons { head, tail }
        }
    }

    impl<T: Builder, U: Builder> Cons<T, U> {
        /// TODO thing of a saner way
        pub fn edit(self, f: fn(T) -> T, u: fn(U) -> U) -> Cons<T, U> {
            let Cons { head, tail } = self;

            let head = f(head);
            let tail = u(tail);

            Cons { head, tail }
        }
    }

    impl<T: Buildable, U: Buildable> Buildable for Cons<T, U> {
        type B = Cons<T::B, U::B>;

        fn builder() -> Self::B {
            Cons {
                head: T::builder(),
                tail: U::builder(),
            }
        }
    }

    #[derive(Default, Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct AdditionalExpectedResponse<T: Buildable = Nil> {
        #[serde(default)]
        pub success: bool,
        pub content_type: String,

        #[serde(flatten)]
        pub other: T,
    }

    #[derive(Default)]
    pub struct AdditionalExpectedResponseBuilder<T: Builder = Nil> {
        pub success: bool,
        pub content_type: String,
        pub other: T,
    }

    impl<T: Builder> AdditionalExpectedResponseBuilder<T> {
        pub fn success(mut self, success: bool) -> Self {
            self.success = success;
            self
        }

        pub fn content_type(mut self, ty: impl Into<String>) -> Self {
            self.content_type = ty.into();
            self
        }

        pub fn other(self, f: fn(T) -> T) -> Self {
            let Self {
                success,
                content_type,
                other,
            } = self;
            let other = f(other);

            Self {
                success,
                content_type,
                other,
            }
        }
    }

    impl<T> Buildable for AdditionalExpectedResponse<T>
    where
        T: Buildable,
        <T as Buildable>::B: Builder,
    {
        type B = AdditionalExpectedResponseBuilder<T::B>;

        fn builder() -> Self::B {
            AdditionalExpectedResponseBuilder::default()
        }
    }

    impl<T: Builder> Builder for AdditionalExpectedResponseBuilder<T> {
        type B = AdditionalExpectedResponse<T::B>;

        fn build(self) -> Self::B {
            let AdditionalExpectedResponseBuilder {
                success,
                content_type,
                other,
            } = self;
            let other = other.build();

            AdditionalExpectedResponse {
                success,
                content_type,
                other,
            }
        }
    }

    #[derive(Default, Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct ExpectedResponse<T: Buildable = Nil> {
        pub content_type: String,

        #[serde(flatten)]
        pub other: T,
    }

    #[derive(Default)]
    pub struct ExpectedResponseBuilder<T: Builder = Nil> {
        pub content_type: String,
        pub other: T,
    }

    impl<T: Builder> ExpectedResponseBuilder<T> {
        pub fn content_type(mut self, ty: impl Into<String>) -> Self {
            self.content_type = ty.into();
            self
        }

        pub fn other(self, f: fn(T) -> T) -> Self {
            let Self {
                content_type,
                other,
            } = self;
            let other = f(other);

            Self {
                content_type,
                other,
            }
        }
    }

    impl<T> Buildable for ExpectedResponse<T>
    where
        T: Buildable,
        <T as Buildable>::B: Builder,
    {
        type B = ExpectedResponseBuilder<T::B>;

        fn builder() -> Self::B {
            ExpectedResponseBuilder::default()
        }
    }

    impl<T: Builder> Builder for ExpectedResponseBuilder<T> {
        type B = ExpectedResponse<T::B>;

        fn build(self) -> Self::B {
            let ExpectedResponseBuilder {
                content_type,
                other,
            } = self;
            let other = other.build();

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
    pub struct Form<T: Buildable = Nil, E: Buildable = Nil> {
        #[serde(default)]
        pub op: DefaultedFormOperations,

        // FIXME: use AnyURI
        pub href: String,

        #[serde(default = "Form::<Nil>::default_content_type")]
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

    impl<T: Buildable> Form<T> {
        pub(crate) const fn default_content_type() -> Cow<'static, str> {
            Cow::Borrowed("application/json")
        }
    }

    impl<T: Buildable> Default for Form<T> {
        fn default() -> Self {
            Self {
                op: Default::default(),
                href: Default::default(),
                content_type: Cow::Borrowed("application/json"),
                content_coding: Default::default(),
                subprotocol: Default::default(),
                security: Default::default(),
                scopes: Default::default(),
                response: Default::default(),
                additional_response: Default::default(),
                other: Default::default(),
            }
        }
    }

    #[serde_as]
    #[skip_serializing_none]
    #[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct InteractionAffordance<T = Nil, F = Nil, R = Nil>
    where
        T: Buildable,
        F: Buildable,
        R: Buildable,
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

    #[test]
    fn build_additional_response() {
        let b = mini::AdditionalExpectedResponse::<Nil>::builder()
            .content_type("text/foo")
            .build();

        dbg!(&b);

        let b = mini::AdditionalExpectedResponse::<super::Response>::builder()
            .content_type("text/bar")
            .other(|v| v.status_code_value(201))
            .build();

        dbg!(&b);

        let b = mini::AdditionalExpectedResponse::<Cons<super::Response, Nil>>::builder()
            .content_type("text/baz")
            .other(|v| v.edit(|v| v.status_code_value(201), |v| v))
            .build();

        dbg!(&b);
    }
    #[test]
    fn build_response() {
        let b = mini::ExpectedResponse::<Nil>::builder()
            .content_type("text/foo")
            .build();

        dbg!(&b);

        let b = mini::ExpectedResponse::<super::Response>::builder()
            .content_type("text/bar")
            .other(|v| v.status_code_value(201))
            .build();

        dbg!(&b);

        let b = mini::ExpectedResponse::<Cons<super::Response, Nil>>::builder()
            .content_type("text/baz")
            .other(|v| v.edit(|v| v.status_code_value(201), |v| v))
            .build();

        dbg!(&b);
    }

    fn deserialize_form(s: &str) {
        let f: super::Form = serde_json::from_str(s).unwrap();

        dbg!(&f);

        let f: mini::Form = serde_json::from_str(s).unwrap();

        dbg!(&f);

        let f: mini::Form<super::Form> = serde_json::from_str(s).unwrap();

        dbg!(&f);

        //        let f: mini::Form<Cons<super::Form, Nil>> = serde_json::from_str(s).unwrap();

        //        dbg!(&f);

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
