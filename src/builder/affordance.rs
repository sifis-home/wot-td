use std::{collections::HashMap, ops::Not};

use serde_json::Value;

use crate::thing::{
    ActionAffordance, DataSchema, EventAffordance, Form, InteractionAffordance, PropertyAffordance,
    SecurityScheme,
};

use super::{
    data_schema::{
        impl_delegate_buildable_data_schema, impl_delegate_schema_builder_like, DataSchemaBuilder,
        EnumerableDataSchema, PartialDataSchema, ReadableWriteableDataSchema,
        SpecializableDataSchema, UnionDataSchema,
    },
    human_readable_info::{
        impl_delegate_buildable_hr_info, BuildableHumanReadableInfo, HumanReadableInfo,
    },
    Error, FormBuilder, MultiLanguageBuilder,
};

pub(super) struct AffordanceBuilder<Affordance> {
    pub(super) name: String,
    pub(super) affordance: Affordance,
}

pub trait BuildableInteractionAffordance {
    fn form<F>(self, f: F) -> Self
    where
        F: FnOnce(FormBuilder<()>) -> FormBuilder<String>;
    fn uri_variable<F, T>(self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>;
}

#[derive(Default)]
pub struct PartialInteractionAffordanceBuilder {
    pub(super) forms: Vec<FormBuilder<String>>,
    pub(super) uri_variables: HashMap<String, DataSchema>,
}

#[derive(Default)]
pub struct InteractionAffordanceBuilder {
    pub(super) partial: PartialInteractionAffordanceBuilder,
    pub(super) info: HumanReadableInfo,
}

impl BuildableInteractionAffordance for PartialInteractionAffordanceBuilder {
    fn form<F>(mut self, f: F) -> Self
    where
        F: FnOnce(FormBuilder<()>) -> FormBuilder<String>,
    {
        self.forms.push(f(FormBuilder::new()));
        self
    }

    fn uri_variable<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        self.uri_variables
            .insert(name.into(), f(DataSchemaBuilder::default()).into());
        self
    }
}

macro_rules! impl_buildable_interaction_affordance {
    ($($ty:ident $( <$($generic:ident),+> )? on $($interaction_path:ident).+),+ $(,)?) => {
        $(
            impl $(< $($generic),+ >)? BuildableInteractionAffordance for $ty $(< $($generic),+ >)? {
                fn form<F>(mut self, f: F) -> Self
                where
                    F: FnOnce(FormBuilder<()>) -> FormBuilder<String>,
                {
                    self.$($interaction_path).* = self.$($interaction_path).*.form(f);
                    self
                }

                fn uri_variable<F, T>(mut self, name: impl Into<String>, f: F) -> Self
                where
                    F: FnOnce(DataSchemaBuilder) -> T,
                    T: Into<DataSchema>,
                {
                    self.$($interaction_path).* = self.$($interaction_path).*.uri_variable(name, f);
                    self
                }
            }
        )+
    };
}

impl_buildable_interaction_affordance!(
    InteractionAffordanceBuilder on partial,
    PropertyAffordanceBuilder<DS> on interaction,
    ActionAffordanceBuilder<I, O> on interaction.partial,
    EventAffordanceBuilder<SS, DS, CS, RS> on interaction.partial,
);

#[derive(Default)]
pub struct PropertyAffordanceBuilder<DataSchema> {
    pub(super) interaction: PartialInteractionAffordanceBuilder,
    pub(super) info: HumanReadableInfo,
    pub(super) data_schema: DataSchema,
    pub(super) observable: Option<bool>,
}

#[derive(Default)]
pub struct ActionAffordanceBuilder<InputSchema, OutputSchema> {
    pub(super) interaction: InteractionAffordanceBuilder,
    pub(super) input: InputSchema,
    pub(super) output: OutputSchema,
    pub(super) safe: bool,
    pub(super) idempotent: bool,
    pub(super) synchronous: Option<bool>,
}

#[derive(Default)]
pub struct EventAffordanceBuilder<
    SubscriptionSchema,
    DataSchema,
    CancellationSchema,
    ResponseSchema,
> {
    pub(super) interaction: InteractionAffordanceBuilder,
    pub(super) subscription: SubscriptionSchema,
    pub(super) data: DataSchema,
    pub(super) cancellation: CancellationSchema,
    pub(super) data_response: ResponseSchema,
}

pub(super) type UsablePropertyAffordanceBuilder = PropertyAffordanceBuilder<DataSchema>;
pub(super) type UsableActionAffordanceBuilder =
    ActionAffordanceBuilder<Option<DataSchema>, Option<DataSchema>>;
pub(super) type UsableEventAffordanceBuilder = EventAffordanceBuilder<
    Option<DataSchema>,
    Option<DataSchema>,
    Option<DataSchema>,
    Option<DataSchema>,
>;

impl_delegate_buildable_data_schema!(
    PropertyAffordanceBuilder<DS>: data_schema,
);

impl_delegate_buildable_hr_info!(
    InteractionAffordanceBuilder on info,
    PropertyAffordanceBuilder<DS> on info,
    ActionAffordanceBuilder<I, O> on interaction,
    EventAffordanceBuilder<SS, DS, CS, RS> on interaction,
);

impl<DataSchema> PropertyAffordanceBuilder<DataSchema> {
    pub fn observable(mut self, value: bool) -> Self {
        self.observable = Some(value);
        self
    }
}

macro_rules! impl_property_affordance_builder_delegator {
    ($($name:ident $( ( $($arg:ident : $arg_ty:ty),+ ) )? -> $ty:ty),+ $(,)?) => {
        $(
            fn $name(self $(, $($arg: $arg_ty),+)?) -> $ty {
                let Self {
                    interaction,
                    info,
                    data_schema,
                    observable,
                } = self;

                let data_schema = data_schema.$name($($($arg),+)?);
                PropertyAffordanceBuilder {
                    interaction,
                    info,
                    data_schema,
                    observable,
                }
            }
        )+
    };
}

impl<DataSchema> SpecializableDataSchema for PropertyAffordanceBuilder<DataSchema>
where
    DataSchema: SpecializableDataSchema,
{
    type Stateless = PropertyAffordanceBuilder<DataSchema::Stateless>;
    type Array = PropertyAffordanceBuilder<DataSchema::Array>;
    type Number = PropertyAffordanceBuilder<DataSchema::Number>;
    type Integer = PropertyAffordanceBuilder<DataSchema::Integer>;
    type Object = PropertyAffordanceBuilder<DataSchema::Object>;
    type String = PropertyAffordanceBuilder<DataSchema::String>;
    type Constant = PropertyAffordanceBuilder<DataSchema::Constant>;

    impl_property_affordance_builder_delegator!(
        array -> Self::Array,
        bool -> Self::Stateless,
        number -> Self::Number,
        integer -> Self::Integer,
        object -> Self::Object,
        string -> Self::String,
        null -> Self::Stateless,
        constant(value: impl Into<Value>) -> Self::Constant,
    );
}

impl<DataSchema> EnumerableDataSchema for PropertyAffordanceBuilder<DataSchema>
where
    DataSchema: EnumerableDataSchema,
{
    type Target = PropertyAffordanceBuilder<DataSchema::Target>;

    fn enumeration(self, value: impl Into<Value>) -> Self::Target {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
        } = self;

        let data_schema = data_schema.enumeration(value);

        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
        }
    }
}

impl<DS> UnionDataSchema for PropertyAffordanceBuilder<DS>
where
    DS: UnionDataSchema,
{
    type Target = PropertyAffordanceBuilder<DS::Target>;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
        } = self;

        let data_schema = data_schema.one_of(f);
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
        }
    }
}

impl<DS> ReadableWriteableDataSchema for PropertyAffordanceBuilder<DS>
where
    DS: ReadableWriteableDataSchema,
{
    type ReadOnly = PropertyAffordanceBuilder<DS::ReadOnly>;
    type WriteOnly = PropertyAffordanceBuilder<DS::WriteOnly>;

    fn read_only(self) -> Self::ReadOnly {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
        } = self;

        let data_schema = data_schema.read_only();
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
        }
    }

    fn write_only(self) -> Self::WriteOnly {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
        } = self;

        let data_schema = data_schema.write_only();
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
        }
    }
}

impl_delegate_schema_builder_like!(PropertyAffordanceBuilder<DS> on data_schema);

impl<OutputSchema> ActionAffordanceBuilder<(), OutputSchema> {
    pub fn input<F, T>(self, f: F) -> ActionAffordanceBuilder<DataSchema, OutputSchema>
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self {
            interaction,
            input: (),
            output,
            safe,
            idempotent,
            synchronous,
        } = self;
        let input = f(DataSchemaBuilder::default()).into();

        ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
        }
    }
}

impl<InputSchema> ActionAffordanceBuilder<InputSchema, ()> {
    pub fn output<F, T>(self, f: F) -> ActionAffordanceBuilder<InputSchema, DataSchema>
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self {
            interaction,
            input,
            output: (),
            safe,
            idempotent,
            synchronous,
        } = self;
        let output = f(DataSchemaBuilder::default()).into();

        ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
        }
    }
}

impl<InputSchema, OutputSchema> ActionAffordanceBuilder<InputSchema, OutputSchema> {
    pub fn safe(mut self) -> Self {
        self.safe = true;
        self
    }

    pub fn idempotent(mut self) -> Self {
        self.idempotent = true;
        self
    }

    pub fn synchronous(mut self, value: bool) -> Self {
        self.synchronous = Some(value);
        self
    }
}

impl<DS, CancellationSchema, ResponseSchema>
    EventAffordanceBuilder<(), DS, CancellationSchema, ResponseSchema>
{
    pub fn subscription<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<DataSchema, DS, CancellationSchema, ResponseSchema>
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self {
            interaction,
            subscription: (),
            data,
            cancellation,
            data_response,
        } = self;
        let subscription = f(DataSchemaBuilder::default()).into();

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        }
    }
}

impl<SubscriptionSchema, CancellationSchema, ResponseSchema>
    EventAffordanceBuilder<SubscriptionSchema, (), CancellationSchema, ResponseSchema>
{
    pub fn data<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<SubscriptionSchema, DataSchema, CancellationSchema, ResponseSchema>
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self {
            interaction,
            subscription,
            data: (),
            cancellation,
            data_response,
        } = self;
        let data = f(DataSchemaBuilder::default()).into();

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        }
    }
}

impl<SubscriptionSchema, DS, ResponseSchema>
    EventAffordanceBuilder<SubscriptionSchema, DS, (), ResponseSchema>
{
    pub fn cancellation<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<SubscriptionSchema, DS, DataSchema, ResponseSchema>
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self {
            interaction,
            subscription,
            data,
            cancellation: (),
            data_response,
        } = self;
        let cancellation = f(DataSchemaBuilder::default()).into();

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        }
    }
}

impl<SubscriptionSchema, DS, CancellationSchema>
    EventAffordanceBuilder<SubscriptionSchema, DS, CancellationSchema, ()>
{
    pub fn data_response<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<SubscriptionSchema, DS, CancellationSchema, DataSchema>
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response: (),
        } = self;
        let data_response = f(DataSchemaBuilder::default()).into();

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        }
    }
}

impl From<InteractionAffordanceBuilder> for InteractionAffordance {
    fn from(builder: InteractionAffordanceBuilder) -> Self {
        let InteractionAffordanceBuilder {
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
            partial:
                PartialInteractionAffordanceBuilder {
                    forms,
                    uri_variables,
                },
        } = builder;

        let forms = forms.into_iter().map(Form::from).collect();
        let uri_variables = uri_variables.is_empty().not().then(|| uri_variables);

        Self {
            attype,
            title,
            titles,
            description,
            descriptions,
            forms,
            uri_variables,
        }
    }
}

trait IntoOptionDataSchema: Sized {
    fn into_option_data_schema(self) -> Option<DataSchema>;
}

impl IntoOptionDataSchema for () {
    #[inline(always)]
    fn into_option_data_schema(self) -> Option<DataSchema> {
        None
    }
}

impl IntoOptionDataSchema for DataSchema {
    #[inline(always)]
    fn into_option_data_schema(self) -> Option<DataSchema> {
        Some(self)
    }
}

impl<DS> From<PropertyAffordanceBuilder<DS>> for UsablePropertyAffordanceBuilder
where
    DS: Into<PartialDataSchema>,
{
    fn from(builder: PropertyAffordanceBuilder<DS>) -> Self {
        let PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
        } = builder;

        let PartialDataSchema {
            constant,
            unit,
            one_of,
            enumeration,
            read_only,
            write_only,
            format,
            subtype,
        } = data_schema.into();

        let HumanReadableInfo {
            attype,
            title,
            titles,
            description,
            descriptions,
        } = info.clone();

        let data_schema = DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant,
            unit,
            one_of,
            enumeration,
            read_only,
            write_only,
            format,
            subtype,
        };

        Self {
            interaction,
            info,
            data_schema,
            observable,
        }
    }
}

impl From<UsablePropertyAffordanceBuilder> for PropertyAffordance {
    fn from(builder: UsablePropertyAffordanceBuilder) -> Self {
        let PropertyAffordanceBuilder {
            interaction:
                PartialInteractionAffordanceBuilder {
                    forms,
                    uri_variables,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
            data_schema,
            observable,
        } = builder;

        let forms = forms.into_iter().map(Form::from).collect();
        let uri_variables = uri_variables.is_empty().not().then(|| uri_variables);

        let interaction = InteractionAffordance {
            attype,
            title,
            titles,
            description,
            descriptions,
            forms,
            uri_variables,
        };

        Self {
            interaction,
            data_schema,
            observable,
        }
    }
}

impl<InputSchema, OutputSchema> From<ActionAffordanceBuilder<InputSchema, OutputSchema>>
    for UsableActionAffordanceBuilder
where
    InputSchema: IntoOptionDataSchema,
    OutputSchema: IntoOptionDataSchema,
{
    fn from(builder: ActionAffordanceBuilder<InputSchema, OutputSchema>) -> Self {
        let ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
        } = builder;

        let input = input.into_option_data_schema();
        let output = output.into_option_data_schema();

        Self {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
        }
    }
}

impl From<UsableActionAffordanceBuilder> for ActionAffordance {
    fn from(builder: UsableActionAffordanceBuilder) -> Self {
        let ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
        } = builder;

        let interaction = interaction.into();

        Self {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
        }
    }
}

impl<SubscriptionSchema, DataSchema, CancellationSchema, ResponseSchema>
    From<EventAffordanceBuilder<SubscriptionSchema, DataSchema, CancellationSchema, ResponseSchema>>
    for UsableEventAffordanceBuilder
where
    SubscriptionSchema: IntoOptionDataSchema,
    DataSchema: IntoOptionDataSchema,
    CancellationSchema: IntoOptionDataSchema,
    ResponseSchema: IntoOptionDataSchema,
{
    fn from(
        builder: EventAffordanceBuilder<
            SubscriptionSchema,
            DataSchema,
            CancellationSchema,
            ResponseSchema,
        >,
    ) -> Self {
        let EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        } = builder;

        let subscription = subscription.into_option_data_schema();
        let data = data.into_option_data_schema();
        let cancellation = cancellation.into_option_data_schema();
        let data_response = data_response.into_option_data_schema();

        Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        }
    }
}

impl From<UsableEventAffordanceBuilder> for EventAffordance {
    fn from(builder: UsableEventAffordanceBuilder) -> Self {
        let EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        } = builder;

        let interaction = interaction.into();

        Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
        }
    }
}

pub(super) trait CheckableInteractionAffordanceBuilder {
    fn check(&self, security_definitions: &HashMap<String, SecurityScheme>) -> Result<(), Error>;
}

impl CheckableInteractionAffordanceBuilder for PartialInteractionAffordanceBuilder {
    fn check(&self, security_definitions: &HashMap<String, SecurityScheme>) -> Result<(), Error> {
        check_form_builders(&self.forms, security_definitions)
    }
}

impl CheckableInteractionAffordanceBuilder for InteractionAffordanceBuilder {
    fn check(&self, security_definitions: &HashMap<String, SecurityScheme>) -> Result<(), Error> {
        check_form_builders(&self.partial.forms, security_definitions)
    }
}

pub(super) fn check_form_builders(
    forms: &[FormBuilder<String>],
    security_definitions: &HashMap<String, SecurityScheme>,
) -> Result<(), Error> {
    for form in forms {
        form.security
            .as_ref()
            .map(|securities| {
                securities.iter().try_for_each(|security| {
                    if security_definitions.contains_key(security) {
                        Ok(())
                    } else {
                        Err(Error::UndefinedSecurity(security.clone()))
                    }
                })
            })
            .transpose()?;
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use crate::{
        builder::data_schema::{
            BuildableDataSchema, NumberDataSchemaBuilderLike, PartialDataSchemaBuilder,
        },
        thing::{
            DataSchemaSubtype, DefaultedFormOperations, FormOperation, IntegerSchema, NumberSchema,
            StringSchema,
        },
    };

    use super::*;

    #[test]
    fn empty_iteraction() {
        let affordance: InteractionAffordance = InteractionAffordanceBuilder::default().into();
        assert_eq!(
            affordance,
            InteractionAffordance {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                forms: vec![],
                uri_variables: None,
            },
        );
    }

    #[test]
    fn full_interaction() {
        let affordance: InteractionAffordance = InteractionAffordanceBuilder::default()
            .attype("attype1")
            .attype("attype2")
            .title("title")
            .titles(|b| b.add("it", "title_it").add("en", "title_en"))
            .description("description")
            .descriptions(|b| b.add("it", "description_it").add("en", "description_en"))
            .form(|b| b.href("form1_href").content_type("content_type"))
            .form(|b| {
                b.op(FormOperation::WriteProperty)
                    .op(FormOperation::ReadProperty)
                    .href("form2_href")
            })
            .uri_variable("uri1", |b| b.number())
            .uri_variable("uri2", |b| b.integer())
            .into();
        assert_eq!(
            affordance,
            InteractionAffordance {
                attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
                title: Some("title".to_string()),
                titles: Some(
                    [("it", "title_it"), ("en", "title_en"),]
                        .into_iter()
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .collect()
                ),
                description: Some("description".to_string()),
                descriptions: Some(
                    [("it", "description_it"), ("en", "description_en"),]
                        .into_iter()
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .collect()
                ),
                forms: vec![
                    Form {
                        op: DefaultedFormOperations::Default,
                        href: "form1_href".to_string(),
                        content_type: Cow::Borrowed("content_type"),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    },
                    Form {
                        op: DefaultedFormOperations::Custom(vec![
                            FormOperation::WriteProperty,
                            FormOperation::ReadProperty,
                        ]),
                        href: "form2_href".to_string(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    },
                ],
                uri_variables: Some(
                    [
                        (
                            "uri1".to_string(),
                            DataSchema {
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
                                subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                                    maximum: None,
                                    minimum: None,
                                    multiple_of: None,
                                }))
                            },
                        ),
                        (
                            "uri2".to_string(),
                            DataSchema {
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
                                subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                                    maximum: None,
                                    minimum: None,
                                }))
                            },
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
            },
        );
    }

    #[test]
    fn property_basic() {
        let affordance_builder: UsablePropertyAffordanceBuilder =
            PropertyAffordanceBuilder::<PartialDataSchemaBuilder>::default()
                .title("property")
                .number()
                .observable(true)
                .form(|b| b.href("href"))
                .unit("cm")
                .read_only()
                .minimum(0.)
                .uri_variable("test", |b| b.bool())
                .into();

        let affordance: PropertyAffordance = affordance_builder.into();

        assert_eq!(
            affordance,
            PropertyAffordance {
                interaction: InteractionAffordance {
                    attype: None,
                    title: Some("property".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    forms: vec![Form {
                        op: DefaultedFormOperations::Default,
                        href: "href".to_owned(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    }],
                    uri_variables: Some(
                        [(
                            "test".to_owned(),
                            DataSchema {
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
                                subtype: Some(DataSchemaSubtype::Boolean),
                            }
                        )]
                        .into_iter()
                        .collect()
                    ),
                },
                data_schema: DataSchema {
                    attype: None,
                    title: Some("property".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: None,
                    enumeration: None,
                    read_only: true,
                    write_only: false,
                    format: None,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        maximum: None,
                        minimum: Some(0.),
                        multiple_of: None,
                    }))
                },
                observable: Some(true),
            },
        );
    }

    #[test]
    fn property_enum() {
        let affordance_builder: UsablePropertyAffordanceBuilder =
            PropertyAffordanceBuilder::<PartialDataSchemaBuilder>::default()
                .title("property")
                .enumeration("enum1")
                .write_only()
                .enumeration("enum2")
                .observable(true)
                .form(|b| b.href("href"))
                .unit("cm")
                .into();

        let affordance: PropertyAffordance = affordance_builder.into();

        assert_eq!(
            affordance,
            PropertyAffordance {
                interaction: InteractionAffordance {
                    attype: None,
                    title: Some("property".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    forms: vec![Form {
                        op: DefaultedFormOperations::Default,
                        href: "href".to_owned(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    }],
                    uri_variables: None,
                },
                data_schema: DataSchema {
                    attype: None,
                    title: Some("property".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: None,
                    enumeration: Some(vec!["enum1".into(), "enum2".into()]),
                    read_only: false,
                    write_only: true,
                    format: None,
                    subtype: None,
                },
                observable: Some(true),
            },
        );
    }

    #[test]
    fn property_one_of() {
        let affordance_builder: UsablePropertyAffordanceBuilder =
            PropertyAffordanceBuilder::<PartialDataSchemaBuilder>::default()
                .title("property")
                .one_of(|b| b.number())
                .one_of(|b| b.integer())
                .observable(true)
                .form(|b| b.href("href"))
                .unit("cm")
                .into();

        let affordance: PropertyAffordance = affordance_builder.into();

        assert_eq!(
            affordance,
            PropertyAffordance {
                interaction: InteractionAffordance {
                    attype: None,
                    title: Some("property".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    forms: vec![Form {
                        op: DefaultedFormOperations::Default,
                        href: "href".to_owned(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    }],
                    uri_variables: None,
                },
                data_schema: DataSchema {
                    attype: None,
                    title: Some("property".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: Some(vec![
                        DataSchema {
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
                            subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                                maximum: None,
                                minimum: None,
                                multiple_of: None,
                            }))
                        },
                        DataSchema {
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
                            subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                                maximum: None,
                                minimum: None,
                            }))
                        },
                    ]),
                    enumeration: None,
                    read_only: false,
                    write_only: false,
                    format: None,
                    subtype: None,
                },
                observable: Some(true),
            },
        );
    }

    #[test]
    fn action_partial() {
        let affordance_builder: UsableActionAffordanceBuilder =
            ActionAffordanceBuilder::<(), ()>::default()
                .title("action")
                .safe()
                .input(|b| b.number().unit("cm").read_only().minimum(0.))
                .form(|b| b.href("href"))
                .into();

        let affordance: ActionAffordance = affordance_builder.into();

        assert_eq!(
            affordance,
            ActionAffordance {
                interaction: InteractionAffordance {
                    attype: None,
                    title: Some("action".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    forms: vec![Form {
                        op: DefaultedFormOperations::Default,
                        href: "href".to_owned(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    }],
                    uri_variables: None,
                },
                input: Some(DataSchema {
                    attype: None,
                    title: None,
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: None,
                    enumeration: None,
                    read_only: true,
                    write_only: false,
                    format: None,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        maximum: None,
                        minimum: Some(0.),
                        multiple_of: None,
                    }))
                }),
                output: None,
                safe: true,
                idempotent: false,
                synchronous: None,
            },
        );
    }

    #[test]
    fn action_full() {
        let affordance_builder: UsableActionAffordanceBuilder =
            ActionAffordanceBuilder::<(), ()>::default()
                .title("action")
                .safe()
                .input(|b| b.number().unit("cm").read_only().minimum(0.))
                .idempotent()
                .output(|b| b.number().unit("cm").read_only().minimum(0.))
                .form(|b| b.href("href"))
                .synchronous(true)
                .into();

        let affordance: ActionAffordance = affordance_builder.into();

        assert_eq!(
            affordance,
            ActionAffordance {
                interaction: InteractionAffordance {
                    attype: None,
                    title: Some("action".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    forms: vec![Form {
                        op: DefaultedFormOperations::Default,
                        href: "href".to_owned(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    }],
                    uri_variables: None,
                },
                input: Some(DataSchema {
                    attype: None,
                    title: None,
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: None,
                    enumeration: None,
                    read_only: true,
                    write_only: false,
                    format: None,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        maximum: None,
                        minimum: Some(0.),
                        multiple_of: None,
                    }))
                }),
                output: Some(DataSchema {
                    attype: None,
                    title: None,
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: None,
                    enumeration: None,
                    read_only: true,
                    write_only: false,
                    format: None,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        maximum: None,
                        minimum: Some(0.),
                        multiple_of: None,
                    }))
                }),
                safe: true,
                idempotent: true,
                synchronous: Some(true),
            },
        );
    }

    #[test]
    fn event_partial() {
        let affordance_builder: UsableEventAffordanceBuilder =
            EventAffordanceBuilder::<(), (), (), ()>::default()
                .title("event")
                .data(|b| b.number().unit("cm").read_only().minimum(0.))
                .form(|b| b.href("href"))
                .into();

        let affordance: EventAffordance = affordance_builder.into();

        assert_eq!(
            affordance,
            EventAffordance {
                interaction: InteractionAffordance {
                    attype: None,
                    title: Some("event".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    forms: vec![Form {
                        op: DefaultedFormOperations::Default,
                        href: "href".to_owned(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    }],
                    uri_variables: None,
                },
                subscription: None,
                data: Some(DataSchema {
                    attype: None,
                    title: None,
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: None,
                    enumeration: None,
                    read_only: true,
                    write_only: false,
                    format: None,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        maximum: None,
                        minimum: Some(0.),
                        multiple_of: None,
                    }))
                }),
                cancellation: None,
                data_response: None,
            },
        );
    }

    #[test]
    fn event_full() {
        let affordance_builder: UsableEventAffordanceBuilder =
            EventAffordanceBuilder::<(), (), (), ()>::default()
                .title("event")
                .cancellation(|b| b.integer())
                .data(|b| b.number().unit("cm").read_only().minimum(0.))
                .subscription(|b| b.bool())
                .data_response(|b| b.string())
                .form(|b| b.href("href"))
                .into();

        let affordance: EventAffordance = affordance_builder.into();

        assert_eq!(
            affordance,
            EventAffordance {
                interaction: InteractionAffordance {
                    attype: None,
                    title: Some("event".to_owned()),
                    titles: None,
                    description: None,
                    descriptions: None,
                    forms: vec![Form {
                        op: DefaultedFormOperations::Default,
                        href: "href".to_owned(),
                        content_type: Form::default_content_type(),
                        content_coding: None,
                        subprotocol: None,
                        security: None,
                        scopes: None,
                        response: None,
                    }],
                    uri_variables: None,
                },
                subscription: Some(DataSchema {
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
                }),
                data: Some(DataSchema {
                    attype: None,
                    title: None,
                    titles: None,
                    description: None,
                    descriptions: None,
                    constant: None,
                    unit: Some("cm".to_owned()),
                    one_of: None,
                    enumeration: None,
                    read_only: true,
                    write_only: false,
                    format: None,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        maximum: None,
                        minimum: Some(0.),
                        multiple_of: None,
                    }))
                }),
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
                    subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                        maximum: None,
                        minimum: None,
                    }))
                }),
                data_response: Some(DataSchema {
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
                    subtype: Some(DataSchemaSubtype::String(StringSchema { max_length: None }))
                }),
            },
        );
    }
}
