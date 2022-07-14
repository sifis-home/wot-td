use std::{collections::HashMap, ops::Not};

use serde_json::Value;

use crate::{
    extend::ExtendableThing,
    hlist::Nil,
    thing::{
        ActionAffordance, DataSchema, DataSchemaFromOther, EventAffordance, Form,
        InteractionAffordance, PropertyAffordance, SecurityScheme,
    },
};

use super::{
    data_schema::{
        buildable_data_schema_delegate, impl_inner_delegate_schema_builder_like_array,
        impl_inner_delegate_schema_builder_like_integer,
        impl_inner_delegate_schema_builder_like_number,
        impl_inner_delegate_schema_builder_like_object, ArrayDataSchemaBuilderLike,
        BuildableDataSchema, DataSchemaBuilder, EnumerableDataSchema, IntegerDataSchemaBuilderLike,
        NumberDataSchemaBuilderLike, ObjectDataSchemaBuilderLike, PartialDataSchema,
        ReadableWriteableDataSchema, SpecializableDataSchema, UnionDataSchema,
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

pub trait BuildableInteractionAffordance<Other: ExtendableThing> {
    fn form<F>(self, f: F) -> Self
    where
        F: FnOnce(FormBuilder<Other, ()>) -> FormBuilder<Other, String>,
        Other::ExpectedResponse: Default;

    fn uri_variable<F, T>(self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default;
}

#[derive(Default)]
pub struct PartialInteractionAffordanceBuilder<Other: ExtendableThing> {
    pub(super) forms: Vec<FormBuilder<Other, String>>,
    pub(super) uri_variables: HashMap<String, DataSchemaFromOther<Other>>,
}

#[derive(Default)]
pub struct InteractionAffordanceBuilder<Other: ExtendableThing> {
    pub(super) partial: PartialInteractionAffordanceBuilder<Other>,
    pub(super) info: HumanReadableInfo,
}

impl<Other: ExtendableThing> BuildableInteractionAffordance<Other>
    for PartialInteractionAffordanceBuilder<Other>
{
    fn form<F>(mut self, f: F) -> Self
    where
        F: FnOnce(FormBuilder<Other, ()>) -> FormBuilder<Other, String>,
        Other::ExpectedResponse: Default,
    {
        self.forms.push(f(FormBuilder::new()));
        self
    }

    fn uri_variable<F, T>(mut self, name: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
    {
        self.uri_variables
            .insert(name.into(), f(DataSchemaBuilder::default()).into());
        self
    }
}

macro_rules! impl_buildable_interaction_affordance {
    ($($ty:ident $( <$($generic:ident $(: $bound:path)?),+> )? on $($interaction_path:ident).+),+ $(,)?) => {
        $(
            impl $(< $($generic $(: $bound)?),+ >)? BuildableInteractionAffordance<Other> for $ty $(< $($generic),+ >)? {
                fn form<F>(mut self, f: F) -> Self
                where
                    F: FnOnce(FormBuilder<Other, ()>) -> FormBuilder<Other, String>,
                    Other::ExpectedResponse: Default,
                {
                    self.$($interaction_path).* = self.$($interaction_path).*.form(f);
                    self
                }

                fn uri_variable<F, T>(mut self, name: impl Into<String>, f: F) -> Self
                where
                    F: FnOnce(DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>) -> T,
                    T: Into<DataSchemaFromOther<Other>>,
                    DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
                {
                    self.$($interaction_path).* = self.$($interaction_path).*.uri_variable(name, f);
                    self
                }
            }
        )+
    };
}

impl_buildable_interaction_affordance!(
    InteractionAffordanceBuilder<Other: ExtendableThing> on partial,
    PropertyAffordanceBuilder<Other: ExtendableThing, DS> on interaction,
    ActionAffordanceBuilder<Other: ExtendableThing, I, O> on interaction.partial,
    EventAffordanceBuilder<Other: ExtendableThing, SS, DS, CS, RS> on interaction.partial,
);

#[derive(Default)]
pub struct PropertyAffordanceBuilder<Other: ExtendableThing, DataSchema> {
    pub(super) interaction: PartialInteractionAffordanceBuilder<Other>,
    pub(super) info: HumanReadableInfo,
    pub(super) data_schema: DataSchema,
    pub(super) observable: Option<bool>,
}

#[derive(Default)]
pub struct ActionAffordanceBuilder<Other: ExtendableThing, InputSchema, OutputSchema> {
    pub(super) interaction: InteractionAffordanceBuilder<Other>,
    pub(super) input: InputSchema,
    pub(super) output: OutputSchema,
    pub(super) safe: bool,
    pub(super) idempotent: bool,
    pub(super) synchronous: Option<bool>,
}

#[derive(Default)]
pub struct EventAffordanceBuilder<
    Other: ExtendableThing,
    SubscriptionSchema,
    DataSchema,
    CancellationSchema,
    ResponseSchema,
> {
    pub(super) interaction: InteractionAffordanceBuilder<Other>,
    pub(super) subscription: SubscriptionSchema,
    pub(super) data: DataSchema,
    pub(super) cancellation: CancellationSchema,
    pub(super) data_response: ResponseSchema,
}

pub(super) type UsablePropertyAffordanceBuilder<Other> =
    PropertyAffordanceBuilder<Other, DataSchemaFromOther<Other>>;
pub(super) type UsableActionAffordanceBuilder<Other> = ActionAffordanceBuilder<
    Other,
    Option<DataSchemaFromOther<Other>>,
    Option<DataSchemaFromOther<Other>>,
>;
pub(super) type UsableEventAffordanceBuilder<Other> = EventAffordanceBuilder<
    Other,
    Option<DataSchemaFromOther<Other>>,
    Option<DataSchemaFromOther<Other>>,
    Option<DataSchemaFromOther<Other>>,
    Option<DataSchemaFromOther<Other>>,
>;

impl<Other, CDS> BuildableDataSchema<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>
    for PropertyAffordanceBuilder<Other, CDS>
where
    Other: ExtendableThing,
    CDS: BuildableDataSchema<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
{
    #[inline]
    fn unit(mut self, value: impl Into<String>) -> Self {
        buildable_data_schema_delegate!(self.data_schema -> unit(value))
    }

    #[inline]
    fn format(mut self, value: impl Into<String>) -> Self {
        buildable_data_schema_delegate!(self.data_schema -> format(value))
    }
}

impl_delegate_buildable_hr_info!(
    InteractionAffordanceBuilder<Other: ExtendableThing> on info,
    PropertyAffordanceBuilder<Other: ExtendableThing, DS> on info,
    ActionAffordanceBuilder<Other: ExtendableThing, I, O> on interaction,
    EventAffordanceBuilder<Other: ExtendableThing, SS, DS, CS, RS> on interaction,
);

impl<Other: ExtendableThing, DataSchema> PropertyAffordanceBuilder<Other, DataSchema> {
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

impl<Other, DataSchema, DS, AS, OS> SpecializableDataSchema<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, DataSchema>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    DataSchema: SpecializableDataSchema<DS, AS, OS>,
{
    type Stateless = PropertyAffordanceBuilder<Other, DataSchema::Stateless>;
    type Array = PropertyAffordanceBuilder<Other, DataSchema::Array>;
    type Number = PropertyAffordanceBuilder<Other, DataSchema::Number>;
    type Integer = PropertyAffordanceBuilder<Other, DataSchema::Integer>;
    type Object = PropertyAffordanceBuilder<Other, DataSchema::Object>;
    type String = PropertyAffordanceBuilder<Other, DataSchema::String>;
    type Constant = PropertyAffordanceBuilder<Other, DataSchema::Constant>;

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

impl<Other, DataSchema, DS, AS, OS> EnumerableDataSchema<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, DataSchema>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    DataSchema: EnumerableDataSchema<DS, AS, OS>,
{
    type Target = PropertyAffordanceBuilder<Other, DataSchema::Target>;

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

impl<Other, CDS, DS, AS, OS> UnionDataSchema<DS, AS, OS> for PropertyAffordanceBuilder<Other, CDS>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: UnionDataSchema<DS, AS, OS>,
{
    type Target = PropertyAffordanceBuilder<Other, CDS::Target>;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
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

impl<Other, CDS, DS, AS, OS> ReadableWriteableDataSchema<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: ReadableWriteableDataSchema<DS, AS, OS>,
{
    type ReadOnly = PropertyAffordanceBuilder<Other, CDS::ReadOnly>;
    type WriteOnly = PropertyAffordanceBuilder<Other, CDS::WriteOnly>;

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

impl<Other, CDS, DS, AS, OS> ArrayDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: ArrayDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_array!(data_schema);
}

impl<Other, CDS, DS, AS, OS> NumberDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: NumberDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_number!(data_schema);
}

impl<Other, CDS, DS, AS, OS> IntegerDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: IntegerDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_integer!(data_schema);
}

impl<Other, CDS, DS, AS, OS> ObjectDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: ObjectDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_object!(data_schema);
}

impl<Other: ExtendableThing, OutputSchema> ActionAffordanceBuilder<Other, (), OutputSchema> {
    pub fn input<F, T>(
        self,
        f: F,
    ) -> ActionAffordanceBuilder<Other, DataSchemaFromOther<Other>, OutputSchema>
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
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

impl<Other: ExtendableThing, InputSchema> ActionAffordanceBuilder<Other, InputSchema, ()> {
    pub fn output<F, T>(
        self,
        f: F,
    ) -> ActionAffordanceBuilder<Other, InputSchema, DataSchemaFromOther<Other>>
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
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

impl<Other: ExtendableThing, InputSchema, OutputSchema>
    ActionAffordanceBuilder<Other, InputSchema, OutputSchema>
{
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

impl<Other: ExtendableThing, DS, CancellationSchema, ResponseSchema>
    EventAffordanceBuilder<Other, (), DS, CancellationSchema, ResponseSchema>
{
    pub fn subscription<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<
        Other,
        DataSchemaFromOther<Other>,
        DS,
        CancellationSchema,
        ResponseSchema,
    >
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
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

impl<Other: ExtendableThing, SubscriptionSchema, CancellationSchema, ResponseSchema>
    EventAffordanceBuilder<Other, SubscriptionSchema, (), CancellationSchema, ResponseSchema>
{
    pub fn data<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<
        Other,
        SubscriptionSchema,
        DataSchemaFromOther<Other>,
        CancellationSchema,
        ResponseSchema,
    >
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
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

impl<Other: ExtendableThing, SubscriptionSchema, DS, ResponseSchema>
    EventAffordanceBuilder<Other, SubscriptionSchema, DS, (), ResponseSchema>
{
    pub fn cancellation<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<
        Other,
        SubscriptionSchema,
        DS,
        DataSchemaFromOther<Other>,
        ResponseSchema,
    >
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
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

impl<Other: ExtendableThing, SubscriptionSchema, DS, CancellationSchema>
    EventAffordanceBuilder<Other, SubscriptionSchema, DS, CancellationSchema, ()>
{
    pub fn data_response<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<
        Other,
        SubscriptionSchema,
        DS,
        CancellationSchema,
        DataSchemaFromOther<Other>,
    >
    where
        F: FnOnce(
            DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>,
        ) -> T,
        T: Into<DataSchemaFromOther<Other>>,
        DataSchemaBuilder<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema>: Default,
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

impl<Other> From<InteractionAffordanceBuilder<Other>> for InteractionAffordance<Other>
where
    Other: ExtendableThing,
    Other::Form: Default,
    Other::InteractionAffordance: Default,
{
    fn from(builder: InteractionAffordanceBuilder<Other>) -> Self {
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
            // TODO
            other: Default::default(),
        }
    }
}

trait IntoOptionDataSchema: Sized {
    type DataSchema;
    type ArraySchema;
    type ObjectSchema;

    fn into_option_data_schema(
        self,
    ) -> Option<DataSchema<Self::DataSchema, Self::ArraySchema, Self::ObjectSchema>>;
}

impl IntoOptionDataSchema for () {
    type DataSchema = Nil;
    type ArraySchema = Nil;
    type ObjectSchema = Nil;

    #[inline(always)]
    fn into_option_data_schema(
        self,
    ) -> Option<DataSchema<Self::DataSchema, Self::ArraySchema, Self::ObjectSchema>> {
        None
    }
}

impl<DS, AS, OS> IntoOptionDataSchema for DataSchema<DS, AS, OS> {
    type DataSchema = DS;
    type ArraySchema = AS;
    type ObjectSchema = OS;

    #[inline(always)]
    fn into_option_data_schema(
        self,
    ) -> Option<DataSchema<Self::DataSchema, Self::ArraySchema, Self::ObjectSchema>> {
        Some(self)
    }
}

impl<Other, CDS, DS, AS, OS> From<PropertyAffordanceBuilder<Other, CDS>>
    for UsablePropertyAffordanceBuilder<Other>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: Into<PartialDataSchema<DS, AS, OS>>,
    DS: Default,
{
    fn from(builder: PropertyAffordanceBuilder<Other, CDS>) -> Self {
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
            // TODO
            other: Default::default(),
        };

        Self {
            interaction,
            info,
            data_schema,
            observable,
        }
    }
}

impl<Other> From<UsablePropertyAffordanceBuilder<Other>> for PropertyAffordance<Other>
where
    Other: ExtendableThing,
    Other::Form: Default,
    Other::InteractionAffordance: Default,
    Other::PropertyAffordance: Default,
{
    fn from(builder: UsablePropertyAffordanceBuilder<Other>) -> Self {
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
            // TODO
            other: Default::default(),
        };

        Self {
            interaction,
            data_schema,
            observable,
            // TODO
            other: Default::default(),
        }
    }
}

impl<Other: ExtendableThing, InputSchema, OutputSchema>
    From<ActionAffordanceBuilder<Other, InputSchema, OutputSchema>>
    for UsableActionAffordanceBuilder<Other>
where
    InputSchema: IntoOptionDataSchema<
        DataSchema = Other::DataSchema,
        ArraySchema = Other::ArraySchema,
        ObjectSchema = Other::ObjectSchema,
    >,
    OutputSchema: IntoOptionDataSchema<
        DataSchema = Other::DataSchema,
        ArraySchema = Other::ArraySchema,
        ObjectSchema = Other::ObjectSchema,
    >,
{
    fn from(builder: ActionAffordanceBuilder<Other, InputSchema, OutputSchema>) -> Self {
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

impl<Other> From<UsableActionAffordanceBuilder<Other>> for ActionAffordance<Other>
where
    Other: ExtendableThing,
    Other::Form: Default,
    Other::InteractionAffordance: Default,
    Other::ActionAffordance: Default,
{
    fn from(builder: UsableActionAffordanceBuilder<Other>) -> Self {
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
            // TODO
            other: Default::default(),
        }
    }
}

impl<Other, SubscriptionSchema, DataSchema, CancellationSchema, ResponseSchema>
    From<
        EventAffordanceBuilder<
            Other,
            SubscriptionSchema,
            DataSchema,
            CancellationSchema,
            ResponseSchema,
        >,
    > for UsableEventAffordanceBuilder<Other>
where
    Other: ExtendableThing,
    SubscriptionSchema: IntoOptionDataSchema<
        DataSchema = Other::DataSchema,
        ArraySchema = Other::ArraySchema,
        ObjectSchema = Other::ObjectSchema,
    >,
    DataSchema: IntoOptionDataSchema<
        DataSchema = Other::DataSchema,
        ArraySchema = Other::ArraySchema,
        ObjectSchema = Other::ObjectSchema,
    >,
    CancellationSchema: IntoOptionDataSchema<
        DataSchema = Other::DataSchema,
        ArraySchema = Other::ArraySchema,
        ObjectSchema = Other::ObjectSchema,
    >,
    ResponseSchema: IntoOptionDataSchema<
        DataSchema = Other::DataSchema,
        ArraySchema = Other::ArraySchema,
        ObjectSchema = Other::ObjectSchema,
    >,
{
    fn from(
        builder: EventAffordanceBuilder<
            Other,
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

impl<Other> From<UsableEventAffordanceBuilder<Other>> for EventAffordance<Other>
where
    Other: ExtendableThing,
    Other::Form: Default,
    Other::InteractionAffordance: Default,
    Other::EventAffordance: Default,
{
    fn from(builder: UsableEventAffordanceBuilder<Other>) -> Self {
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
            // TODO
            other: Default::default(),
        }
    }
}

pub(super) trait CheckableInteractionAffordanceBuilder {
    fn check(&self, security_definitions: &HashMap<String, SecurityScheme>) -> Result<(), Error>;
}

impl<Other: ExtendableThing> CheckableInteractionAffordanceBuilder
    for PartialInteractionAffordanceBuilder<Other>
{
    fn check(&self, security_definitions: &HashMap<String, SecurityScheme>) -> Result<(), Error> {
        check_form_builders(&self.forms, security_definitions)
    }
}

impl<Other: ExtendableThing> CheckableInteractionAffordanceBuilder
    for InteractionAffordanceBuilder<Other>
{
    fn check(&self, security_definitions: &HashMap<String, SecurityScheme>) -> Result<(), Error> {
        check_form_builders(&self.partial.forms, security_definitions)
    }
}

pub(super) fn check_form_builders<Other: ExtendableThing>(
    forms: &[FormBuilder<Other, String>],
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
    use crate::{
        builder::data_schema::{
            BuildableDataSchema, NumberDataSchemaBuilderLike, PartialDataSchemaBuilder,
        },
        hlist::Nil,
        thing::{DataSchemaSubtype, DefaultedFormOperations, FormOperation, NumberSchema},
    };

    use super::*;

    #[test]
    fn empty_iteraction() {
        let affordance: InteractionAffordance<Nil> = InteractionAffordanceBuilder::default().into();
        assert_eq!(affordance, InteractionAffordance::default());
    }

    #[test]
    fn full_interaction() {
        let affordance: InteractionAffordance<Nil> = InteractionAffordanceBuilder::default()
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
                        content_type: Some("content_type".to_string()),
                        ..Default::default()
                    },
                    Form {
                        op: DefaultedFormOperations::Custom(vec![
                            FormOperation::WriteProperty,
                            FormOperation::ReadProperty,
                        ]),
                        href: "form2_href".to_string(),
                        ..Default::default()
                    },
                ],
                uri_variables: Some(
                    [
                        (
                            "uri1".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Number(Default::default())),
                                ..Default::default()
                            },
                        ),
                        (
                            "uri2".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Integer(Default::default())),
                                ..Default::default()
                            },
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                other: Default::default(),
            },
        );
    }

    #[test]
    fn property_basic() {
        let affordance_builder: UsablePropertyAffordanceBuilder<Nil> =
            PropertyAffordanceBuilder::<Nil, PartialDataSchemaBuilder<_, _, _>>::default()
                .title("property")
                .number()
                .observable(true)
                .form(|b| b.href("href"))
                .unit("cm")
                .read_only()
                .minimum(0.)
                .uri_variable("test", |b| b.bool())
                .into();

        let affordance: PropertyAffordance<Nil> = affordance_builder.into();

        assert_eq!(
            affordance,
            PropertyAffordance {
                interaction: InteractionAffordance {
                    title: Some("property".to_owned()),
                    forms: vec![Form {
                        href: "href".to_owned(),
                        ..Default::default()
                    }],
                    uri_variables: Some(
                        [(
                            "test".to_owned(),
                            DataSchemaFromOther::<Nil> {
                                subtype: Some(DataSchemaSubtype::Boolean),
                                ..Default::default()
                            }
                        )]
                        .into_iter()
                        .collect()
                    ),
                    ..Default::default()
                },
                data_schema: DataSchemaFromOther::<Nil> {
                    title: Some("property".to_owned()),
                    unit: Some("cm".to_owned()),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(0.),
                        ..Default::default()
                    })),
                    ..Default::default()
                },
                observable: Some(true),
                other: Nil,
            },
        );
    }

    #[test]
    fn property_enum() {
        let affordance_builder: UsablePropertyAffordanceBuilder<Nil> =
            PropertyAffordanceBuilder::<Nil, PartialDataSchemaBuilder<_, _, _>>::default()
                .title("property")
                .enumeration("enum1")
                .write_only()
                .enumeration("enum2")
                .observable(true)
                .form(|b| b.href("href"))
                .unit("cm")
                .into();

        let affordance: PropertyAffordance<Nil> = affordance_builder.into();

        assert_eq!(
            affordance,
            PropertyAffordance {
                interaction: InteractionAffordance {
                    title: Some("property".to_owned()),
                    forms: vec![Form {
                        href: "href".to_owned(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
                data_schema: DataSchemaFromOther::<Nil> {
                    title: Some("property".to_owned()),
                    unit: Some("cm".to_owned()),
                    enumeration: Some(vec!["enum1".into(), "enum2".into()]),
                    write_only: true,
                    ..Default::default()
                },
                observable: Some(true),
                other: Nil,
            },
        );
    }

    #[test]
    fn property_one_of() {
        let affordance_builder: UsablePropertyAffordanceBuilder<Nil> =
            PropertyAffordanceBuilder::<Nil, PartialDataSchemaBuilder<_, _, _>>::default()
                .title("property")
                .one_of(|b| b.number())
                .one_of(|b| b.integer())
                .observable(true)
                .form(|b| b.href("href"))
                .unit("cm")
                .into();

        let affordance: PropertyAffordance<Nil> = affordance_builder.into();

        assert_eq!(
            affordance,
            PropertyAffordance {
                interaction: InteractionAffordance {
                    title: Some("property".to_owned()),
                    forms: vec![Form {
                        href: "href".to_owned(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
                data_schema: DataSchemaFromOther::<Nil> {
                    title: Some("property".to_owned()),
                    unit: Some("cm".to_owned()),
                    one_of: Some(vec![
                        DataSchemaFromOther::<Nil> {
                            subtype: Some(DataSchemaSubtype::Number(Default::default())),
                            ..Default::default()
                        },
                        DataSchemaFromOther::<Nil> {
                            subtype: Some(DataSchemaSubtype::Integer(Default::default())),
                            ..Default::default()
                        },
                    ]),
                    ..Default::default()
                },
                observable: Some(true),
                other: Nil,
            },
        );
    }

    #[test]
    fn action_partial() {
        let affordance_builder: UsableActionAffordanceBuilder<Nil> =
            ActionAffordanceBuilder::<Nil, (), ()>::default()
                .title("action")
                .safe()
                .input(|b| b.number().unit("cm").read_only().minimum(0.))
                .form(|b| b.href("href"))
                .into();

        let affordance: ActionAffordance<Nil> = affordance_builder.into();

        assert_eq!(
            affordance,
            ActionAffordance {
                interaction: InteractionAffordance {
                    title: Some("action".to_owned()),
                    forms: vec![Form {
                        href: "href".to_owned(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
                input: Some(DataSchemaFromOther::<Nil> {
                    unit: Some("cm".to_owned()),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(0.),
                        ..Default::default()
                    })),
                    ..Default::default()
                }),
                safe: true,
                ..Default::default()
            },
        );
    }

    #[test]
    fn action_full() {
        let affordance_builder: UsableActionAffordanceBuilder<Nil> =
            ActionAffordanceBuilder::<Nil, (), ()>::default()
                .title("action")
                .safe()
                .input(|b| b.number().unit("cm").read_only().minimum(0.))
                .idempotent()
                .output(|b| b.number().unit("cm").read_only().minimum(0.))
                .form(|b| b.href("href"))
                .synchronous(true)
                .into();

        let affordance: ActionAffordance<Nil> = affordance_builder.into();

        assert_eq!(
            affordance,
            ActionAffordance {
                interaction: InteractionAffordance {
                    title: Some("action".to_owned()),
                    forms: vec![Form {
                        href: "href".to_owned(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
                input: Some(DataSchemaFromOther::<Nil> {
                    unit: Some("cm".to_owned()),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(0.),
                        ..Default::default()
                    })),
                    ..Default::default()
                }),
                output: Some(DataSchemaFromOther::<Nil> {
                    unit: Some("cm".to_owned()),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(0.),
                        ..Default::default()
                    })),
                    ..Default::default()
                }),
                safe: true,
                idempotent: true,
                synchronous: Some(true),
                other: Nil,
            },
        );
    }

    #[test]
    fn event_partial() {
        let affordance_builder: UsableEventAffordanceBuilder<Nil> =
            EventAffordanceBuilder::<Nil, (), (), (), ()>::default()
                .title("event")
                .data(|b| b.number().unit("cm").read_only().minimum(0.))
                .form(|b| b.href("href"))
                .into();

        let affordance: EventAffordance<Nil> = affordance_builder.into();

        assert_eq!(
            affordance,
            EventAffordance {
                interaction: InteractionAffordance {
                    title: Some("event".to_owned()),
                    forms: vec![Form {
                        href: "href".to_owned(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
                data: Some(DataSchemaFromOther::<Nil> {
                    unit: Some("cm".to_owned()),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(0.),
                        ..Default::default()
                    })),
                    ..Default::default()
                }),
                ..Default::default()
            },
        );
    }

    #[test]
    fn event_full() {
        let affordance_builder: UsableEventAffordanceBuilder<Nil> =
            EventAffordanceBuilder::<Nil, (), (), (), ()>::default()
                .title("event")
                .cancellation(|b| b.integer())
                .data(|b| b.number().unit("cm").read_only().minimum(0.))
                .subscription(|b| b.bool())
                .data_response(|b| b.string())
                .form(|b| b.href("href"))
                .into();

        let affordance: EventAffordance<Nil> = affordance_builder.into();

        assert_eq!(
            affordance,
            EventAffordance {
                interaction: InteractionAffordance {
                    title: Some("event".to_owned()),
                    forms: vec![Form {
                        href: "href".to_owned(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
                subscription: Some(DataSchemaFromOther::<Nil> {
                    subtype: Some(DataSchemaSubtype::Boolean),
                    ..Default::default()
                }),
                data: Some(DataSchemaFromOther::<Nil> {
                    unit: Some("cm".to_owned()),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(0.),
                        ..Default::default()
                    })),
                    ..Default::default()
                }),
                cancellation: Some(DataSchemaFromOther::<Nil> {
                    subtype: Some(DataSchemaSubtype::Integer(Default::default())),
                    ..Default::default()
                }),
                data_response: Some(DataSchemaFromOther::<Nil> {
                    subtype: Some(DataSchemaSubtype::String(Default::default())),
                    ..Default::default()
                }),
                other: Nil,
            },
        );
    }
}
