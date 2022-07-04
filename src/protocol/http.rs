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
    pub fn headers(&mut self, header: MessageHeader) -> &mut Self {
        self.headers.push(header);
        self
    }
    pub fn status_code_value(&mut self, value: usize) -> &mut Self {
        self.status_code_value = Some(value);
        self
    }
}

impl Builder for ResponseBuilder {
    type B = Response;

    fn build(&self) -> Response {
        let ResponseBuilder {
            ref headers,
            ref status_code_value,
        } = self;
        let headers = headers.clone();
        let status_code_value = status_code_value.clone();
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

#[derive(Debug, Default)]
struct FormBuilder {
    method_name: Option<Method>,
}

impl FormBuilder {
    pub fn method(&mut self, method_name: Method) -> &mut Self {
        self.method_name = Some(method_name);

        self
    }
}

impl Builder for FormBuilder {
    type B = Form;

    fn build(&self) -> Form {
        let FormBuilder { ref method_name } = self;
        let method_name = method_name.clone();

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

        fn build(&self) -> Self::B;
    }

    pub trait Buildable: Default {
        type B: Builder;

        fn builder() -> Self::B;
    }

    impl Builder for Nil {
        type B = Nil;

        fn build(&self) -> Nil {
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

        fn build(&self) -> Self::B {
            let Cons { ref head, ref tail } = self;
            let head = head.build();
            let tail = tail.build();

            Cons { head, tail }
        }
    }

    impl<T: Builder, U: Builder> Cons<T, U> {
        pub fn edit(&mut self, f: fn(&mut T) -> &mut T) -> &mut Self {
            f(&mut self.head);
            self
        }

        // TODO: move it in the main trait as next_mut
        pub fn next(&mut self) -> &mut U {
            &mut self.tail
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
        pub fn success(&mut self, success: bool) -> &mut Self {
            self.success = success;
            self
        }

        pub fn content_type(&mut self, ty: impl Into<String>) -> &mut Self {
            self.content_type = ty.into();
            self
        }

        pub fn other(&mut self, f: fn(&mut T) -> &mut T) -> &mut Self {
            f(&mut self.other);

            self
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

        fn build(&self) -> Self::B {
            let AdditionalExpectedResponseBuilder {
                ref success,
                ref content_type,
                ref other,
            } = self;
            let success = success.clone();
            let content_type = content_type.clone();
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
        pub fn content_type(&mut self, ty: impl Into<String>) -> &mut Self {
            self.content_type = ty.into();
            self
        }

        pub fn other(&mut self, f: fn(&mut T) -> &mut T) -> &mut Self {
            f(&mut self.other);
            self
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

        fn build(&self) -> Self::B {
            let ExpectedResponseBuilder {
                ref content_type,
                ref other,
            } = self;
            let content_type = content_type.clone();
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

    impl<T: Buildable, E: Buildable> Form<T, E> {
        pub(crate) const fn default_content_type() -> Cow<'static, str> {
            Cow::Borrowed("application/json")
        }
    }

    impl<T: Buildable, E: Buildable> Default for Form<T, E> {
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

    impl<T, E> Buildable for Form<T, E>
    where
        T: Buildable,
        <T as Buildable>::B: Builder,
        E: Buildable,
        <E as Buildable>::B: Builder,
    {
        type B = FormBuilder<T::B, E::B>;

        fn builder() -> Self::B {
            FormBuilder::default()
        }
    }

    pub struct FormBuilder<T: Builder, E: Builder> {
        pub op: DefaultedFormOperations,
        pub href: String,
        pub content_type: Cow<'static, str>,
        pub content_coding: Option<String>,
        pub subprotocol: Option<String>,
        pub security: Option<Vec<String>>,
        pub scopes: Option<Vec<String>>,
        pub response: Option<ExpectedResponseBuilder<E>>,
        pub additional_response: Option<AdditionalExpectedResponseBuilder<E>>,
        pub other: T,
    }

    impl<T: Builder, E: Builder> FormBuilder<T, E> {
        pub fn op(&mut self, op: DefaultedFormOperations) -> &mut Self {
            self.op = op;
            self
        }
        pub fn href(&mut self, href: impl Into<String>) -> &mut Self {
            self.href = href.into();
            self
        }
        pub fn response(
            &mut self,
            f: fn(&mut ExpectedResponseBuilder<E>) -> &mut ExpectedResponseBuilder<E>,
        ) -> &mut Self {
            let r = self.response.get_or_insert_with(Default::default);
            f(r);
            self
        }
        pub fn additional_response(
            &mut self,
            f: fn(
                &mut AdditionalExpectedResponseBuilder<E>,
            ) -> &mut AdditionalExpectedResponseBuilder<E>,
        ) -> &mut Self {
            let a = self
                .additional_response
                .get_or_insert_with(Default::default);

            f(a);
            self
        }
        pub fn other(&mut self, f: fn(&mut T) -> &mut T) -> &mut Self {
            f(&mut self.other);
            self
        }
    }

    impl<T: Builder, E: Builder> Default for FormBuilder<T, E> {
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

    impl<T: Builder, E: Builder> Builder for FormBuilder<T, E> {
        type B = Form<T::B, E::B>;

        fn build(&self) -> Self::B {
            let Self {
                ref op,
                ref href,
                ref content_type,
                ref content_coding,
                ref subprotocol,
                ref security,
                ref scopes,
                ref response,
                ref additional_response,
                ref other,
            } = self;

            let op = op.clone();
            let href = href.clone();
            let content_type = content_type.clone();
            let content_coding = content_coding.clone();
            let subprotocol = subprotocol.clone();
            let security = security.clone();
            let scopes = scopes.clone();
            let response = response.as_ref().map(|r| r.build());
            let additional_response = additional_response.as_ref().map(|a| a.build());
            let other = other.build();

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

    #[derive(Default)]
    pub struct InteractionAffordanceBuilder<T, F, R>
    where
        T: Builder,
        F: Builder,
        R: Builder,
    {
        attype: Option<Vec<String>>,
        title: Option<String>,
        description: Option<String>,
        forms: Vec<FormBuilder<F, R>>,
        other: T,
    }

    impl<T, F, R> InteractionAffordanceBuilder<T, F, R>
    where
        T: Builder,
        F: Builder,
        R: Builder,
    {
        pub fn attype(&mut self, attype: impl Into<String>) -> &mut Self {
            self.attype
                .get_or_insert_with(Default::default)
                .push(attype.into());
            self
        }
        pub fn form(
            &mut self,
            f: fn(&mut FormBuilder<F, R>) -> &mut FormBuilder<F, R>,
        ) -> &mut Self {
            let mut form = Default::default();
            f(&mut form);
            self.forms.push(form);
            self
        }
        pub fn other(&mut self, f: fn(&mut T) -> &mut T) -> &mut Self {
            f(&mut self.other);

            self
        }
    }

    impl<T: Builder, F: Builder, R: Builder> Builder for InteractionAffordanceBuilder<T, F, R> {
        type B = InteractionAffordance<T::B, F::B, R::B>;

        fn build(&self) -> Self::B {
            let InteractionAffordanceBuilder {
                attype,
                title,
                description,
                forms,
                other,
            } = self;

            let attype = attype.clone();
            let title = title.clone();
            let description = description.clone();
            let forms = forms.into_iter().map(|f| f.build()).collect();
            let other = other.build();

            Self::B {
                attype,
                title,
                description,
                forms,
                other,
            }
        }
    }

    impl<T: Buildable, F: Buildable, R: Buildable> Buildable for InteractionAffordance<T, F, R> {
        type B = InteractionAffordanceBuilder<T::B, F::B, R::B>;

        fn builder() -> Self::B {
            InteractionAffordanceBuilder::default()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hlist::{Cons, Nil};

    #[test]
    fn build_affordance() {
        let a = mini::InteractionAffordance::<Nil>::builder()
            .attype("OnOff")
            .attype("OtherType")
            .form(|f| f.href("/my/form"))
            .form(|f| f.href("/blah").response(|r| r.content_type("test/bar")))
            .build();

        dbg!(&a);

        let a = mini::InteractionAffordance::<Nil, super::Form, super::Response>::builder()
            .attype("OnOff")
            .attype("OtherType")
            .form(|f| {
                f.href("/my/form")
                    .other(|v| v.method(super::Method::Patch))
                    .response(|r| r.other(|v| v.status_code_value(200)))
            })
            .form(|f| f.href("/blah").response(|r| r.content_type("test/bar")))
            .build();

        dbg!(&a);
    }

    #[test]
    fn build_form() {
        let f = mini::Form::<Nil>::builder()
            .href("/my/form")
            .response(|r| r.content_type("text/bar"))
            .additional_response(|a| a.success(true))
            .build();
        dbg!(&f);

        let f = mini::Form::<Nil, super::Response>::builder()
            .href("/my/form")
            .response(|r| {
                r.content_type("text/bar")
                    .other(|v| v.status_code_value(200))
            })
            .additional_response(|a| a.other(|v| v.status_code_value(400)))
            .build();

        dbg!(&f);

        let f = mini::Form::<super::Form, super::Response>::builder()
            .href("/my/form")
            .response(|r| {
                r.content_type("text/bar")
                    .other(|v| v.status_code_value(200))
            })
            .additional_response(|a| a.other(|v| v.status_code_value(400)))
            .other(|v| v.method(super::Method::Patch))
            .build();

        dbg!(&f);
    }

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
            .other(|v| v.edit(|v| v.status_code_value(201)))
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
            .other(|v| v.edit(|v| v.status_code_value(201)))
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
