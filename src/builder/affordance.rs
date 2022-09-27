//! The builder elements related to affordances.

use std::{collections::HashMap, ops::Not};

use serde_json::Value;

use crate::{
    extend::{Extend, Extendable, ExtendableThing},
    thing::{
        ActionAffordance, DataSchema, DefaultedFormOperations, EventAffordance, Form,
        FormOperation, InteractionAffordance, PropertyAffordance, SecurityScheme,
    },
};

use super::{
    data_schema::{
        buildable_data_schema_delegate, impl_inner_delegate_schema_builder_like_array,
        impl_inner_delegate_schema_builder_like_integer,
        impl_inner_delegate_schema_builder_like_number,
        impl_inner_delegate_schema_builder_like_object, uri_variables_contains_arrays_objects,
        ArrayDataSchemaBuilderLike, BuildableDataSchema, DataSchemaBuilder, EnumerableDataSchema,
        IntegerDataSchemaBuilderLike, NumberDataSchemaBuilderLike, ObjectDataSchemaBuilderLike,
        PartialDataSchema, PartialDataSchemaBuilder, ReadableWriteableDataSchema,
        SpecializableDataSchema, UncheckedDataSchemaFromOther, UncheckedDataSchemaMap,
        UnionDataSchema,
    },
    human_readable_info::{
        impl_delegate_buildable_hr_info, BuildableHumanReadableInfo, HumanReadableInfo,
    },
    AffordanceType, Error, Extended, FormBuilder, MultiLanguageBuilder, ToExtend,
};

/// A conversion into an _usable_ form of a value.
///
/// This is extremely handy to convert between the same kind with different generics. [`From`] and
/// [`Into`] cannot be used directly because of some blanket implementations that cause conflicts.
pub trait IntoUsable<T>: Sized {
    /// Converts `self` into an _usable_ value with a different type.
    fn into_usable(self) -> T;
}

pub(super) struct AffordanceBuilder<Affordance> {
    pub(super) name: String,
    pub(super) affordance: Affordance,
}

/// An interface for a buildable version of an [`InteractionAffordance`](crate::thing::InteractionAffordance).
///
/// In order to model the specification, each type that can be created using a builder pattern and
/// that _behaves_ like an `InteractionAffordance` should implement this trait.
///
/// # Notes
///
/// This trait *should not* be implemented directly, even if it is not sealed.
pub trait BuildableInteractionAffordance<Other: ExtendableThing> {
    /// Adds a new `Form`.
    ///
    /// It takes a function that accepts an _incomplete_ [`FormBuilder`] and must return a
    /// _complete_ one.
    fn form<F>(self, f: F) -> Self
    where
        F: FnOnce(
            FormBuilder<Other, (), <Other::Form as Extendable>::Empty>,
        ) -> FormBuilder<Other, String, Other::Form>,
        Other::Form: Extendable;

    /// Adds a new _URI variable_.
    ///
    /// It takes a function that accepts a `DataSchema` builder and must return a
    /// type convertible into a `DataSchema`.
    fn uri_variable<F, T>(self, name: impl Into<String>, f: F) -> Self
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
        Other::DataSchema: Extendable;
}

/// _Partial_ variant of an [`InteractionAffordanceBuilder`].
///
/// This variant is necessary for building a [`PropertyAffordance`], which is composed of a set of
/// _human readable_ fields shared between [`InteractionAffordance`] and [`DataSchema`].
pub(crate) struct PartialInteractionAffordanceBuilder<
    Other: ExtendableThing,
    OtherInteractionAffordance,
> {
    pub(super) forms: Vec<FormBuilder<Other, String, Other::Form>>,
    pub(super) uri_variables: HashMap<String, UncheckedDataSchemaFromOther<Other>>,

    /// Partial interaction affordance extension.
    pub other: OtherInteractionAffordance,
}

impl<Other, OtherInteractionAffordance> Default
    for PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>
where
    Other: ExtendableThing,
    OtherInteractionAffordance: Default,
{
    fn default() -> Self {
        Self {
            forms: Default::default(),
            uri_variables: Default::default(),
            other: Default::default(),
        }
    }
}

impl<Other>
    PartialInteractionAffordanceBuilder<Other, <Other::InteractionAffordance as Extendable>::Empty>
where
    Other: ExtendableThing,
    Other::InteractionAffordance: Extendable,
{
    pub(crate) fn empty() -> Self {
        Self {
            forms: Default::default(),
            uri_variables: Default::default(),
            other: Other::InteractionAffordance::empty(),
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance>
    PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>
{
    /// Extends the current type, passing a closure that returns `T`.
    fn ext_with<F, T>(
        self,
        f: F,
    ) -> PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance::Target>
    where
        OtherInteractionAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            forms,
            uri_variables,
            other,
        } = self;
        let other = other.ext_with(f);
        PartialInteractionAffordanceBuilder {
            forms,
            uri_variables,
            other,
        }
    }
}

impl<Other, OtherInteractionAffordance>
    IntoUsable<PartialInteractionAffordanceBuilder<Other, Other::InteractionAffordance>>
    for PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>
where
    Other: ExtendableThing,
    OtherInteractionAffordance: Into<Other::InteractionAffordance>,
{
    fn into_usable(
        self,
    ) -> PartialInteractionAffordanceBuilder<Other, Other::InteractionAffordance> {
        let Self {
            forms,
            uri_variables,
            other,
        } = self;

        let other = other.into();
        PartialInteractionAffordanceBuilder {
            forms,
            uri_variables,
            other,
        }
    }
}

/// A builder for [`InteractionAffordance`].
#[derive(Default)]
pub(crate) struct InteractionAffordanceBuilder<Other: ExtendableThing, OtherInteractionAffordance> {
    pub(super) partial: PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>,
    pub(super) info: HumanReadableInfo,
}

impl<Other: ExtendableThing, OtherInteractionAffordance>
    InteractionAffordanceBuilder<Other, OtherInteractionAffordance>
{
    /// Extends the current type, passing a closure that returns `T`.
    pub(crate) fn ext_with<F, T>(
        self,
        f: F,
    ) -> InteractionAffordanceBuilder<Other, OtherInteractionAffordance::Target>
    where
        OtherInteractionAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self { partial, info } = self;
        let partial = partial.ext_with(f);
        InteractionAffordanceBuilder { partial, info }
    }

    /// Extend the current extension with an additional element
    #[cfg(test)]
    #[inline]
    pub(crate) fn ext<T>(
        self,
        t: T,
    ) -> InteractionAffordanceBuilder<Other, OtherInteractionAffordance::Target>
    where
        OtherInteractionAffordance: Extend<T>,
    {
        self.ext_with(|| t)
    }
}

impl<Other> InteractionAffordanceBuilder<Other, Other::InteractionAffordance>
where
    Other: ExtendableThing,
    Other::InteractionAffordance: Extendable,
{
    pub(crate) fn empty(
    ) -> InteractionAffordanceBuilder<Other, <Other::InteractionAffordance as Extendable>::Empty>
    {
        InteractionAffordanceBuilder {
            partial: PartialInteractionAffordanceBuilder::empty(),
            info: Default::default(),
        }
    }
}

impl<Other, OtherInteractionAffordance> BuildableInteractionAffordance<Other>
    for PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>
where
    Other: ExtendableThing,
    Other::Form: Extendable,
{
    fn form<F>(mut self, f: F) -> Self
    where
        F: FnOnce(
            FormBuilder<Other, (), <Other::Form as Extendable>::Empty>,
        ) -> FormBuilder<Other, String, Other::Form>,
    {
        self.forms.push(f(FormBuilder::new()));
        self
    }

    fn uri_variable<F, T>(mut self, name: impl Into<String>, f: F) -> Self
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
        self.uri_variables.insert(
            name.into(),
            f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into(),
        );
        self
    }
}

macro_rules! impl_buildable_interaction_affordance {
    ($($ty:ident $( <$($generic:ident $(: $bound:path)?),+> )? on $($interaction_path:ident).+),+ $(,)?) => {
        $(
            impl $(< $($generic $(: $bound)?),+ >)? BuildableInteractionAffordance<Other> for $ty $(< $($generic),+ >)?
            where
                Other::Form: Extendable
            {
                fn form<F>(mut self, f: F) -> Self
                where
                    F: FnOnce(FormBuilder<Other, (), <Other::Form as Extendable>::Empty>) -> FormBuilder<Other, String, Other::Form>,
                    Other::Form: Extendable,
                {
                    self.$($interaction_path).* = self.$($interaction_path).*.form(f);
                    self
                }

                fn uri_variable<F, T>(mut self, name: impl Into<String>, f: F) -> Self
                where
                    F: FnOnce(DataSchemaBuilder<<Other::DataSchema as Extendable>::Empty, Other::ArraySchema, Other::ObjectSchema, ToExtend>) -> T,
                    T: Into<UncheckedDataSchemaFromOther<Other>>,
                    Other::DataSchema: Extendable,
                {
                    self.$($interaction_path).* = self.$($interaction_path).*.uri_variable(name, f);
                    self
                }
            }
        )+
    };
}

impl_buildable_interaction_affordance!(
    InteractionAffordanceBuilder<Other: ExtendableThing, OtherInteractionAffordance> on partial,
    PropertyAffordanceBuilder<Other: ExtendableThing, DS, OtherInteractionAffordance, OtherPropertyAffordance> on interaction,
    ActionAffordanceBuilder<Other: ExtendableThing, OtherInteractionAffordance, OtherActionAffordance> on interaction.partial,
    EventAffordanceBuilder<Other: ExtendableThing, OtherInteractionAffordance, OtherEventAffordance> on interaction.partial,
);

/// A builder for [`PropertyAffordance`]
pub struct PropertyAffordanceBuilder<
    Other: ExtendableThing,
    DataSchema,
    OtherInteractionAffordance,
    OtherPropertyAffordance,
> {
    pub(super) interaction: PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>,
    pub(super) info: HumanReadableInfo,
    pub(super) data_schema: DataSchema,
    pub(super) observable: Option<bool>,

    /// Property affordance extension.
    pub other: OtherPropertyAffordance,
}

impl<Other, DataSchema, OtherInteractionAffordance, OtherPropertyAffordance> Default
    for PropertyAffordanceBuilder<
        Other,
        DataSchema,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
where
    Other: ExtendableThing,
    PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>: Default,
    DataSchema: Default,
    OtherPropertyAffordance: Default,
{
    fn default() -> Self {
        Self {
            interaction: Default::default(),
            info: Default::default(),
            data_schema: Default::default(),
            observable: Default::default(),
            other: Default::default(),
        }
    }
}

/// Builder for [`ActionAffordance`].
pub struct ActionAffordanceBuilder<
    Other: ExtendableThing,
    OtherInteractionAffordance,
    OtherActionAffordance,
> {
    pub(super) interaction: InteractionAffordanceBuilder<Other, OtherInteractionAffordance>,
    pub(super) input: Option<UncheckedDataSchemaFromOther<Other>>,
    pub(super) output: Option<UncheckedDataSchemaFromOther<Other>>,
    pub(super) safe: bool,
    pub(super) idempotent: bool,
    pub(super) synchronous: Option<bool>,

    /// Action affordance extension.
    pub other: OtherActionAffordance,
}

impl<Other, OtherInteractionAffordance, OtherActionAffordance> Default
    for ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
where
    Other: ExtendableThing,
    InteractionAffordanceBuilder<Other, OtherInteractionAffordance>: Default,
    OtherActionAffordance: Default,
{
    fn default() -> Self {
        Self {
            interaction: Default::default(),
            input: Default::default(),
            output: Default::default(),
            safe: Default::default(),
            idempotent: Default::default(),
            synchronous: Default::default(),
            other: Default::default(),
        }
    }
}

impl<Other> ActionAffordanceBuilder<Other, Other::InteractionAffordance, Other::ActionAffordance>
where
    Other: ExtendableThing,
    Other::InteractionAffordance: Extendable,
    Other::ActionAffordance: Extendable,
{
    pub(crate) fn empty() -> ActionAffordanceBuilder<
        Other,
        <Other::InteractionAffordance as Extendable>::Empty,
        <Other::ActionAffordance as Extendable>::Empty,
    > {
        ActionAffordanceBuilder {
            interaction: InteractionAffordanceBuilder::empty(),
            input: Default::default(),
            output: Default::default(),
            safe: Default::default(),
            idempotent: Default::default(),
            synchronous: Default::default(),
            other: Other::ActionAffordance::empty(),
        }
    }
}

/// Builder for [`EventAffordance`].
#[derive(Default)]
pub struct EventAffordanceBuilder<
    Other: ExtendableThing,
    OtherInteractionAffordance,
    OtherEventAffordance,
> {
    pub(super) interaction: InteractionAffordanceBuilder<Other, OtherInteractionAffordance>,
    pub(super) subscription: Option<UncheckedDataSchemaFromOther<Other>>,
    pub(super) data: Option<UncheckedDataSchemaFromOther<Other>>,
    pub(super) cancellation: Option<UncheckedDataSchemaFromOther<Other>>,
    pub(super) data_response: Option<UncheckedDataSchemaFromOther<Other>>,

    /// Event affordance extension.
    pub other: OtherEventAffordance,
}

type EmptyEventAffordanceBuilder<Other> = EventAffordanceBuilder<
    Other,
    <<Other as ExtendableThing>::InteractionAffordance as Extendable>::Empty,
    <<Other as ExtendableThing>::EventAffordance as Extendable>::Empty,
>;

impl<Other> EventAffordanceBuilder<Other, Other::InteractionAffordance, Other::EventAffordance>
where
    Other: ExtendableThing,
    Other::InteractionAffordance: Extendable,
    Other::EventAffordance: Extendable,
{
    pub(crate) fn empty() -> EmptyEventAffordanceBuilder<Other> {
        EventAffordanceBuilder {
            interaction: InteractionAffordanceBuilder::empty(),
            subscription: Default::default(),
            data: Default::default(),
            cancellation: Default::default(),
            data_response: Default::default(),
            other: Other::EventAffordance::empty(),
        }
    }
}

impl<Other, OtherInteractionAffordance, OtherEventAffordance>
    IntoUsable<UsableEventAffordanceBuilder<Other>>
    for EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
where
    Other: ExtendableThing,
    OtherInteractionAffordance: Into<Other::InteractionAffordance>,
    OtherEventAffordance: Into<Other::EventAffordance>,
{
    fn into_usable(self) -> UsableEventAffordanceBuilder<Other> {
        let Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        } = self;

        let interaction = {
            let InteractionAffordanceBuilder {
                partial:
                    PartialInteractionAffordanceBuilder {
                        forms,
                        uri_variables,
                        other,
                    },
                info,
            } = interaction;
            let other = other.into();
            let partial = PartialInteractionAffordanceBuilder {
                forms,
                uri_variables,
                other,
            };
            InteractionAffordanceBuilder { partial, info }
        };
        let other = other.into();

        UsableEventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        }
    }
}

pub(super) type UsablePropertyAffordanceBuilder<Other> = PropertyAffordanceBuilder<
    Other,
    PartialDataSchema<
        <Other as ExtendableThing>::DataSchema,
        <Other as ExtendableThing>::ArraySchema,
        <Other as ExtendableThing>::ObjectSchema,
    >,
    <Other as ExtendableThing>::InteractionAffordance,
    <Other as ExtendableThing>::PropertyAffordance,
>;
pub(super) type UsableActionAffordanceBuilder<Other> = ActionAffordanceBuilder<
    Other,
    <Other as ExtendableThing>::InteractionAffordance,
    <Other as ExtendableThing>::ActionAffordance,
>;
pub(super) type UsableEventAffordanceBuilder<Other> = EventAffordanceBuilder<
    Other,
    <Other as ExtendableThing>::InteractionAffordance,
    <Other as ExtendableThing>::EventAffordance,
>;

impl<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance, Status>
    BuildableDataSchema<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema, Status>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing,
    CDS: BuildableDataSchema<Other::DataSchema, Other::ArraySchema, Other::ObjectSchema, Status>,
{
    #[inline]
    fn unit(mut self, value: impl Into<String>) -> Self {
        buildable_data_schema_delegate!(self.data_schema -> unit(value))
    }

    #[inline]
    fn format(mut self, value: impl Into<String>) -> Self {
        buildable_data_schema_delegate!(self.data_schema -> format(value))
    }

    #[inline]
    fn default_value(mut self, value: impl Into<Value>) -> Self {
        buildable_data_schema_delegate!(self.data_schema -> default_value(value))
    }
}

impl_delegate_buildable_hr_info!(
    InteractionAffordanceBuilder<Other: ExtendableThing, OtherInteractionAffordance> on info,
    PropertyAffordanceBuilder<Other: ExtendableThing, DS, OtherInteractionAffordance, OtherPropertyAffordance> on info,
    ActionAffordanceBuilder<Other: ExtendableThing, OtherInteractionAffordance, OtherPropertyAffordance> on interaction,
    EventAffordanceBuilder<Other: ExtendableThing, Affordance, OtherPropertyAffordance> on interaction,
);

impl<Other>
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
    >
where
    Other: ExtendableThing,
    Other::DataSchema: Extendable,
    Other::InteractionAffordance: Extendable,
    Other::PropertyAffordance: Extendable,
{
    pub(crate) fn empty() -> Self {
        Self {
            interaction: PartialInteractionAffordanceBuilder::empty(),
            info: Default::default(),
            data_schema: PartialDataSchemaBuilder::<Other::DataSchema, _, _, _>::empty(),
            observable: Default::default(),
            other: Other::PropertyAffordance::empty(),
        }
    }
}

impl<Other: ExtendableThing, DS, OtherInteractionAffordance, OtherPropertyAffordance>
    PropertyAffordanceBuilder<Other, DS, OtherInteractionAffordance, OtherPropertyAffordance>
{
    /// Extends the interaction affordance, passing a closure that returns `T`.
    pub fn ext_interaction_with<F, T>(
        self,
        f: F,
    ) -> PropertyAffordanceBuilder<
        Other,
        DS,
        OtherInteractionAffordance::Target,
        OtherPropertyAffordance,
    >
    where
        OtherInteractionAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;
        let interaction = interaction.ext_with(f);
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }

    /// Extends the interaction affordance with an additional element.
    #[inline]
    pub fn ext_interaction<T>(
        self,
        t: T,
    ) -> PropertyAffordanceBuilder<
        Other,
        DS,
        OtherInteractionAffordance::Target,
        OtherPropertyAffordance,
    >
    where
        OtherInteractionAffordance: Extend<T>,
    {
        self.ext_interaction_with(|| t)
    }

    /// Extends the property affordance, passing a closure that returns `T`.
    pub fn ext_with<F, T>(
        self,
        f: F,
    ) -> PropertyAffordanceBuilder<
        Other,
        DS,
        OtherInteractionAffordance,
        OtherPropertyAffordance::Target,
    >
    where
        OtherPropertyAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;
        let other = other.ext_with(f);
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }

    /// Extends the property affordance with an additional element.
    #[inline]
    pub fn ext<T>(
        self,
        t: T,
    ) -> PropertyAffordanceBuilder<
        Other,
        DS,
        OtherInteractionAffordance,
        OtherPropertyAffordance::Target,
    >
    where
        OtherPropertyAffordance: Extend<T>,
    {
        self.ext_with(|| t)
    }
}

impl<Other, OtherInteractionAffordance, OtherPropertyAffordance, DS, AS, OS>
    PropertyAffordanceBuilder<
        Other,
        PartialDataSchemaBuilder<DS, AS, OS, ToExtend>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
where
    Other: ExtendableThing,
{
    /// Extends the data schema, passing a closure that returns `T`.
    pub fn ext_data_schema_with<F, T>(
        self,
        f: F,
    ) -> PropertyAffordanceBuilder<
        Other,
        PartialDataSchemaBuilder<DS::Target, AS, OS, ToExtend>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
    where
        F: FnOnce() -> T,
        DS: Extend<T>,
    {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;
        let data_schema = data_schema.ext_with(f);
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }

    /// Extends the data schema with an additional element.
    #[inline]
    pub fn ext_data_schema<T>(
        self,
        t: T,
    ) -> PropertyAffordanceBuilder<
        Other,
        PartialDataSchemaBuilder<DS::Target, AS, OS, ToExtend>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
    where
        DS: Extend<T>,
    {
        self.ext_data_schema_with(|| t)
    }

    /// Makes the builder unextendable and allows further customizations.
    pub fn finish_extend_data_schema(
        self,
    ) -> PropertyAffordanceBuilder<
        Other,
        PartialDataSchemaBuilder<DS, AS, OS, Extended>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    > {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;
        let data_schema = data_schema.finish_extend();
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }
}

impl<Other, OtherInteractionAffordance, OtherPropertyAffordance, DS, AS, OS>
    PropertyAffordanceBuilder<
        Other,
        DataSchemaBuilder<DS, AS, OS, ToExtend>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
where
    Other: ExtendableThing,
{
    /// Extends the data schema, passing a closure that returns `T`.
    pub fn ext_data_schema_with<F, T>(
        self,
        f: F,
    ) -> PropertyAffordanceBuilder<
        Other,
        DataSchemaBuilder<DS::Target, AS, OS, ToExtend>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
    where
        F: FnOnce() -> T,
        DS: Extend<T>,
    {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;
        let data_schema = data_schema.ext_with(f);
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }

    /// Extends the data schema with an additional element.
    #[inline]
    pub fn ext_data_schema<T>(
        self,
        t: T,
    ) -> PropertyAffordanceBuilder<
        Other,
        DataSchemaBuilder<DS::Target, AS, OS, ToExtend>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
    where
        DS: Extend<T>,
    {
        self.ext_data_schema_with(|| t)
    }

    /// Makes the builder unextendable and allows further customizations.
    pub fn finish_extend_data_schema(
        self,
    ) -> PropertyAffordanceBuilder<
        Other,
        DataSchemaBuilder<DS, AS, OS, Extended>,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    > {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;
        let data_schema = data_schema.finish_extend();
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }
}

impl<Other, CDS, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    IntoUsable<UsablePropertyAffordanceBuilder<Other>>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: Into<PartialDataSchema<DS, AS, OS>>,
    PartialInteractionAffordanceBuilder<Other, OtherInteractionAffordance>:
        IntoUsable<PartialInteractionAffordanceBuilder<Other, Other::InteractionAffordance>>,
    OtherPropertyAffordance: Into<Other::PropertyAffordance>,
{
    fn into_usable(self) -> UsablePropertyAffordanceBuilder<Other> {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;

        let interaction = interaction.into_usable();
        let data_schema = data_schema.into();
        let other = other.into();
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherActionAffordance>
    ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
{
    /// Extends the interaction affordance, passing a closure that returns `T`.
    pub fn ext_interaction_with<F, T>(
        self,
        f: F,
    ) -> ActionAffordanceBuilder<Other, OtherInteractionAffordance::Target, OtherActionAffordance>
    where
        OtherInteractionAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        } = self;
        let interaction = interaction.ext_with(f);
        ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        }
    }

    /// Extends the interaction affordance with an additional element.
    #[inline]
    pub fn ext_interaction<T>(
        self,
        t: T,
    ) -> ActionAffordanceBuilder<Other, OtherInteractionAffordance::Target, OtherActionAffordance>
    where
        OtherInteractionAffordance: Extend<T>,
    {
        self.ext_interaction_with(|| t)
    }

    /// Extends the action affordance, passing a closure that returns `T`.
    pub fn ext_with<F, T>(
        self,
        f: F,
    ) -> ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance::Target>
    where
        OtherActionAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        } = self;
        let other = other.ext_with(f);
        ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        }
    }

    /// Extends the action affordance with an additional element.
    #[inline]
    pub fn ext<T>(
        self,
        t: T,
    ) -> ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance::Target>
    where
        OtherActionAffordance: Extend<T>,
    {
        self.ext_with(|| t)
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherEventAffordance>
    EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
{
    /// Extends the interaction affordance, passing a closure that returns `T`.
    pub fn ext_interaction_with<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance::Target, OtherEventAffordance>
    where
        OtherInteractionAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        } = self;
        let interaction = interaction.ext_with(f);
        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        }
    }

    /// Extends the interaction affordance with an additional element.
    #[inline]
    pub fn ext_interaction<T>(
        self,
        t: T,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance::Target, OtherEventAffordance>
    where
        OtherInteractionAffordance: Extend<T>,
    {
        self.ext_interaction_with(|| t)
    }

    /// Extends the event affordance, passing a closure that returns `T`.
    pub fn ext_with<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance::Target>
    where
        OtherEventAffordance: Extend<T>,
        F: FnOnce() -> T,
    {
        let Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        } = self;
        let other = other.ext_with(f);
        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        }
    }

    /// Extends the event affordance with an additional element.
    #[inline]
    pub fn ext<T>(
        self,
        t: T,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance::Target>
    where
        OtherEventAffordance: Extend<T>,
    {
        self.ext_with(|| t)
    }
}

impl<Other: ExtendableThing, DataSchema, OtherInteractionAffordance, OtherPropertyAffordance>
    PropertyAffordanceBuilder<
        Other,
        DataSchema,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
{
    /// Sets the value of the `observable` field.
    pub fn observable(mut self, value: bool) -> Self {
        self.observable = Some(value);
        self
    }
}

macro_rules! impl_property_affordance_builder_delegator {
    ($($name:ident $(< $($generic_def:ident),+ >)? $( ( $($arg:ident : $arg_ty:ty),+ ) )? $(where $($generic:ident : $bound:path),+)? => $ty:ty),+ $(,)?) => {
        $(
            fn $name $(<$($generic_def),+>)? (self $(, $($arg: $arg_ty),+)?) -> $ty
            $(
                where
                    $($generic: $bound),+
            )?
            {
                let Self {
                    interaction,
                    info,
                    data_schema,
                    observable,
                    other,
                } = self;

                let data_schema = data_schema.$name($($($arg),+)?);
                PropertyAffordanceBuilder {
                    interaction,
                    info,
                    data_schema,
                    observable,
                    other,
                }
            }
        )+
    };
}

impl<Other, DataSchema, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    SpecializableDataSchema<DS, AS, OS>
    for PropertyAffordanceBuilder<
        Other,
        DataSchema,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    DataSchema: SpecializableDataSchema<DS, AS, OS>,
{
    type Stateless = PropertyAffordanceBuilder<
        Other,
        DataSchema::Stateless,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;
    type Array = PropertyAffordanceBuilder<
        Other,
        DataSchema::Array,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;
    type Number = PropertyAffordanceBuilder<
        Other,
        DataSchema::Number,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;
    type Integer = PropertyAffordanceBuilder<
        Other,
        DataSchema::Integer,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;
    type Object = PropertyAffordanceBuilder<
        Other,
        DataSchema::Object,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;
    type String = PropertyAffordanceBuilder<
        Other,
        DataSchema::String,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;
    type Constant = PropertyAffordanceBuilder<
        Other,
        DataSchema::Constant,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;

    impl_property_affordance_builder_delegator!(
        array where AS: Default => Self::Array,
        array_ext<F>(f: F) where F: FnOnce(AS::Empty) -> AS, AS: Extendable => Self::Array,
        bool => Self::Stateless,
        number => Self::Number,
        integer => Self::Integer,
        object where OS: Default => Self::Object,
        object_ext<F>(f: F) where F: FnOnce(OS::Empty) -> OS, OS: Extendable => Self::Object,
        string => Self::String,
        null => Self::Stateless,
        constant(value: impl Into<Value>) => Self::Constant,
    );
}

impl<Other, DataSchema, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    EnumerableDataSchema<DS, AS, OS, Extended>
    for PropertyAffordanceBuilder<
        Other,
        DataSchema,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    DataSchema: EnumerableDataSchema<DS, AS, OS, Extended>,
{
    type Target = PropertyAffordanceBuilder<
        Other,
        DataSchema::Target,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;

    fn enumeration(self, value: impl Into<Value>) -> Self::Target {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;

        let data_schema = data_schema.enumeration(value);

        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }
}

impl<Other, CDS, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    UnionDataSchema<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: UnionDataSchema<DS, AS, OS>,
{
    type Target = PropertyAffordanceBuilder<
        Other,
        CDS::Target,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(
            DataSchemaBuilder<
                <DS as Extendable>::Empty,
                Other::ArraySchema,
                Other::ObjectSchema,
                ToExtend,
            >,
        ) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchemaFromOther<Other>>,
    {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;

        let data_schema = data_schema.one_of(f);
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }
}

impl<Other, CDS, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    ReadableWriteableDataSchema<DS, AS, OS, Extended>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: ReadableWriteableDataSchema<DS, AS, OS, Extended>,
{
    type ReadOnly = PropertyAffordanceBuilder<
        Other,
        CDS::ReadOnly,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;
    type WriteOnly = PropertyAffordanceBuilder<
        Other,
        CDS::WriteOnly,
        OtherInteractionAffordance,
        OtherPropertyAffordance,
    >;

    fn read_only(self) -> Self::ReadOnly {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;

        let data_schema = data_schema.read_only();
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }

    fn write_only(self) -> Self::WriteOnly {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;

        let data_schema = data_schema.write_only();
        PropertyAffordanceBuilder {
            interaction,
            info,
            data_schema,
            observable,
            other,
        }
    }
}

impl<Other, CDS, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    ArrayDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: ArrayDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_array!(data_schema);
}

impl<Other, CDS, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    NumberDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: NumberDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_number!(data_schema);
}

impl<Other, CDS, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    IntegerDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: IntegerDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_integer!(data_schema);
}

impl<Other, CDS, DS, AS, OS, OtherInteractionAffordance, OtherPropertyAffordance>
    ObjectDataSchemaBuilderLike<DS, AS, OS>
    for PropertyAffordanceBuilder<Other, CDS, OtherInteractionAffordance, OtherPropertyAffordance>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
    CDS: ObjectDataSchemaBuilderLike<DS, AS, OS>,
{
    impl_inner_delegate_schema_builder_like_object!(data_schema);
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherActionAffordance>
    ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
{
    /// Adds a new _input_ schema.
    ///
    /// It takes a function that accepts a `DataSchema` builder and must return a
    /// type convertible into a `DataSchema`.
    pub fn input<F, T>(
        self,
        f: F,
    ) -> ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
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
        let Self {
            interaction,
            input: _,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        } = self;
        let input = Some(f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into());

        ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherActionAffordance>
    ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
{
    /// Adds a new _output_ schema.
    ///
    /// It takes a function that accepts a `DataSchema` builder and must return a
    /// type convertible into a `DataSchema`.
    pub fn output<F, T>(
        self,
        f: F,
    ) -> ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
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
        let Self {
            interaction,
            input,
            output: _,
            safe,
            idempotent,
            synchronous,
            other,
        } = self;
        let output = Some(f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into());

        ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherActionAffordance>
    ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
{
    /// Sets the value of the `safe` field to `true`.
    pub fn safe(mut self) -> Self {
        self.safe = true;
        self
    }

    /// Sets the value of the `idempotent` field to `true`.
    pub fn idempotent(mut self) -> Self {
        self.idempotent = true;
        self
    }

    /// Sets the value of the `synchronous` field.
    pub fn synchronous(mut self, value: bool) -> Self {
        self.synchronous = Some(value);
        self
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherEventAffordance>
    EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
{
    /// Adds a new _subscription_ schema.
    ///
    /// It takes a function that accepts a `DataSchema` builder and must return a
    /// type convertible into a `DataSchema`.
    pub fn subscription<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
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
        let Self {
            interaction,
            subscription: _,
            data,
            cancellation,
            data_response,
            other,
        } = self;
        let subscription = Some(f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into());

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherEventAffordance>
    EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
{
    /// Adds a new _data_ schema.
    ///
    /// It takes a function that accepts a `DataSchema` builder and must return a
    /// type convertible into a `DataSchema`.
    pub fn data<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
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
        let Self {
            interaction,
            subscription,
            data: _,
            cancellation,
            data_response,
            other,
        } = self;
        let data = Some(f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into());

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherEventAffordance>
    EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
{
    /// Adds a new _cancellation_ schema.
    ///
    /// It takes a function that accepts a `DataSchema` builder and must return a
    /// type convertible into a `DataSchema`.
    pub fn cancellation<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
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
        let Self {
            interaction,
            subscription,
            data,
            cancellation: _,
            data_response,
            other,
        } = self;
        let cancellation = Some(f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into());

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherEventAffordance>
    EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
{
    /// Adds a new _data response_ schema.
    ///
    /// It takes a function that accepts a `DataSchema` builder and must return a
    /// type convertible into a `DataSchema`.
    pub fn data_response<F, T>(
        self,
        f: F,
    ) -> EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
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
        let Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response: _,
            other,
        } = self;
        let data_response =
            Some(f(DataSchemaBuilder::<Other::DataSchema, _, _, _>::empty()).into());

        EventAffordanceBuilder {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        }
    }
}

impl<Other, OtherInteractionAffordance>
    From<InteractionAffordanceBuilder<Other, OtherInteractionAffordance>>
    for UncheckedInteractionAffordance<Other>
where
    Other: ExtendableThing,
    OtherInteractionAffordance: Into<Other::InteractionAffordance>,
{
    fn from(builder: InteractionAffordanceBuilder<Other, OtherInteractionAffordance>) -> Self {
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
                    other,
                },
        } = builder;

        let forms = forms.into_iter().map(Form::from).collect();
        let uri_variables = uri_variables.is_empty().not().then_some(uri_variables);
        let other = other.into();

        Self {
            attype,
            title,
            titles,
            description,
            descriptions,
            forms,
            uri_variables,
            other,
        }
    }
}

impl<Other: ExtendableThing, OtherInteractionAffordance, OtherActionAffordance>
    IntoUsable<UsableActionAffordanceBuilder<Other>>
    for ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
where
    OtherInteractionAffordance: Into<Other::InteractionAffordance>,
    OtherActionAffordance: Into<Other::ActionAffordance>,
{
    fn into_usable(self) -> UsableActionAffordanceBuilder<Other> {
        let ActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        } = self;

        let interaction = {
            let InteractionAffordanceBuilder {
                partial:
                    PartialInteractionAffordanceBuilder {
                        forms,
                        uri_variables,
                        other,
                    },
                info,
            } = interaction;
            let other = other.into();
            let partial = PartialInteractionAffordanceBuilder {
                forms,
                uri_variables,
                other,
            };
            InteractionAffordanceBuilder { partial, info }
        };

        let other = other.into();

        UsableActionAffordanceBuilder {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        }
    }
}

pub(super) trait CheckableInteractionAffordanceBuilder {
    fn check<F>(
        &self,
        security_definitions: &HashMap<String, SecurityScheme>,
        affordance_type: AffordanceType,
        is_allowed_op: F,
    ) -> Result<(), Error>
    where
        F: Fn(FormOperation) -> bool;
}

impl<Other: ExtendableThing> CheckableInteractionAffordanceBuilder
    for PartialInteractionAffordanceBuilder<Other, Other::InteractionAffordance>
{
    fn check<F>(
        &self,
        security_definitions: &HashMap<String, SecurityScheme>,
        affordance_type: AffordanceType,
        is_allowed_op: F,
    ) -> Result<(), Error>
    where
        F: Fn(FormOperation) -> bool,
    {
        check_form_builders(
            &self.forms,
            security_definitions,
            affordance_type,
            is_allowed_op,
        )?;
        if uri_variables_contains_arrays_objects::<Other>(&self.uri_variables) {
            return Err(Error::InvalidUriVariables);
        }

        Ok(())
    }
}

impl<Other: ExtendableThing> CheckableInteractionAffordanceBuilder
    for InteractionAffordanceBuilder<Other, Other::InteractionAffordance>
{
    fn check<F>(
        &self,
        security_definitions: &HashMap<String, SecurityScheme>,
        affordance_type: AffordanceType,
        is_allowed_op: F,
    ) -> Result<(), Error>
    where
        F: Fn(FormOperation) -> bool,
    {
        check_form_builders(
            &self.partial.forms,
            security_definitions,
            affordance_type,
            is_allowed_op,
        )?;
        if uri_variables_contains_arrays_objects::<Other>(&self.partial.uri_variables) {
            return Err(Error::InvalidUriVariables);
        }

        Ok(())
    }
}

pub(super) fn check_form_builders<Other, F>(
    forms: &[FormBuilder<Other, String, Other::Form>],
    security_definitions: &HashMap<String, SecurityScheme>,
    affordance_type: AffordanceType,
    is_allowed_op: F,
) -> Result<(), Error>
where
    Other: ExtendableThing,
    F: Fn(FormOperation) -> bool,
{
    for form in forms {
        if let DefaultedFormOperations::Custom(ops) = &form.op {
            let invalid_op = ops.iter().copied().find(|&op| is_allowed_op(op).not());
            if let Some(operation) = invalid_op {
                return Err(Error::InvalidOpInForm {
                    context: affordance_type.into(),
                    operation,
                });
            }
        }

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

pub(crate) struct UncheckedInteractionAffordance<Other: ExtendableThing> {
    attype: Option<Vec<String>>,
    title: Option<String>,
    titles: Option<MultiLanguageBuilder<String>>,
    description: Option<String>,
    descriptions: Option<MultiLanguageBuilder<String>>,
    forms: Vec<Form<Other>>,
    uri_variables: Option<UncheckedDataSchemaMap<Other>>,
    other: Other::InteractionAffordance,
}

impl<Other: ExtendableThing, OtherInteractionAffordance>
    TryFrom<InteractionAffordanceBuilder<Other, OtherInteractionAffordance>>
    for InteractionAffordance<Other>
where
    Other: ExtendableThing,
    OtherInteractionAffordance: Into<Other::InteractionAffordance>,
{
    type Error = Error;

    fn try_from(
        builder: InteractionAffordanceBuilder<Other, OtherInteractionAffordance>,
    ) -> Result<Self, Self::Error> {
        let interaction: UncheckedInteractionAffordance<_> = builder.into();
        interaction.try_into()
    }
}

impl<Other: ExtendableThing> TryFrom<UncheckedInteractionAffordance<Other>>
    for InteractionAffordance<Other>
{
    type Error = Error;

    fn try_from(affordance: UncheckedInteractionAffordance<Other>) -> Result<Self, Self::Error> {
        let UncheckedInteractionAffordance {
            attype,
            title,
            titles,
            description,
            descriptions,
            forms,
            uri_variables,
            other,
        } = affordance;

        let titles = titles.map(|titles| titles.build()).transpose()?;
        let descriptions = descriptions
            .map(|descriptions| descriptions.build())
            .transpose()?;
        let uri_variables = uri_variables
            .map(|uri_variables| {
                uri_variables
                    .into_iter()
                    .map(|(key, value)| value.try_into().map(|value| (key, value)))
                    .collect()
            })
            .transpose()?;

        Ok(Self {
            attype,
            title,
            titles,
            description,
            descriptions,
            forms,
            uri_variables,
            other,
        })
    }
}

pub(crate) trait BuildableAffordance {
    type Target;

    fn build(self) -> Result<Self::Target, Error>;
}

impl<Other, DS, AS, OS> BuildableAffordance for UsablePropertyAffordanceBuilder<Other>
where
    Other: ExtendableThing<DataSchema = DS, ArraySchema = AS, ObjectSchema = OS>,
{
    type Target = PropertyAffordance<Other>;

    fn build(self) -> Result<Self::Target, Error> {
        let Self {
            interaction,
            info,
            data_schema,
            observable,
            other,
        } = self;

        let PartialInteractionAffordanceBuilder {
            forms,
            uri_variables,
            other: other_interaction,
        } = interaction;

        let PartialDataSchema {
            constant,
            default,
            unit,
            one_of,
            enumeration,
            read_only,
            write_only,
            format,
            subtype,
            other: data_schema_other,
        } = data_schema;

        let HumanReadableInfo {
            attype,
            title,
            titles,
            description,
            descriptions,
        } = info;

        let titles = titles.map(|titles| titles.build()).transpose()?;
        let descriptions = descriptions
            .map(|descriptions| descriptions.build())
            .transpose()?;
        let forms = forms.into_iter().map(Into::into).collect();
        let uri_variables = uri_variables
            .is_empty()
            .not()
            .then(|| {
                uri_variables
                    .into_iter()
                    .map(|(key, value)| value.try_into().map(|value| (key, value)))
                    .collect()
            })
            .transpose()?;
        let one_of = one_of
            .map(|one_of| one_of.into_iter().map(TryInto::try_into).collect())
            .transpose()?;
        let subtype = subtype.map(TryInto::try_into).transpose()?;

        let interaction = InteractionAffordance {
            attype: attype.clone(),
            title: title.clone(),
            titles: titles.clone(),
            description: description.clone(),
            descriptions: descriptions.clone(),
            forms,
            uri_variables,
            other: other_interaction,
        };

        let data_schema = DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant,
            default,
            unit,
            one_of,
            enumeration,
            read_only,
            write_only,
            format,
            subtype,
            other: data_schema_other,
        };

        Ok(PropertyAffordance {
            interaction,
            data_schema,
            observable,
            other,
        })
    }
}

impl<Other, OtherInteractionAffordance, OtherEventAffordance> BuildableAffordance
    for EventAffordanceBuilder<Other, OtherInteractionAffordance, OtherEventAffordance>
where
    Other: ExtendableThing,
    OtherInteractionAffordance: Into<Other::InteractionAffordance>,
    OtherEventAffordance: Into<Other::EventAffordance>,
{
    type Target = EventAffordance<Other>;

    fn build(self) -> Result<Self::Target, Error> {
        let Self {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        } = self;

        let interaction = interaction.try_into()?;
        let subscription = subscription
            .map(|subscription| subscription.try_into())
            .transpose()?;
        let data = data.map(|data| data.try_into()).transpose()?;
        let cancellation = cancellation
            .map(|cancellation| cancellation.try_into())
            .transpose()?;
        let data_response = data_response
            .map(|data_response| data_response.try_into())
            .transpose()?;
        let other = other.into();

        Ok(Self::Target {
            interaction,
            subscription,
            data,
            cancellation,
            data_response,
            other,
        })
    }
}

impl<Other, OtherInteractionAffordance, OtherActionAffordance> BuildableAffordance
    for ActionAffordanceBuilder<Other, OtherInteractionAffordance, OtherActionAffordance>
where
    Other: ExtendableThing,
    OtherInteractionAffordance: Into<Other::InteractionAffordance>,
    OtherActionAffordance: Into<Other::ActionAffordance>,
{
    type Target = ActionAffordance<Other>;

    fn build(self) -> Result<Self::Target, Error> {
        let Self {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        } = self;

        let interaction = interaction.try_into()?;
        let input = input.map(|input| input.try_into()).transpose()?;
        let output = output.map(|output| output.try_into()).transpose()?;
        let other = other.into();

        Ok(Self::Target {
            interaction,
            input,
            output,
            safe,
            idempotent,
            synchronous,
            other,
        })
    }
}

#[cfg(test)]
mod test {
    use serde::{Deserialize, Serialize};
    use serde_json::json;

    use crate::{
        builder::data_schema::{
            BuildableDataSchema, NumberDataSchemaBuilderLike, PartialDataSchemaBuilder,
        },
        hlist::{Cons, Nil},
        thing::{
            DataSchemaFromOther, DataSchemaSubtype, DefaultedFormOperations, FormOperation,
            Minimum, NumberSchema,
        },
    };

    use super::*;

    #[test]
    fn empty_iteraction() {
        let affordance: UncheckedInteractionAffordance<Nil> =
            InteractionAffordanceBuilder::<Nil, ()>::default().into();
        let affordance: InteractionAffordance<_> = affordance.try_into().unwrap();
        assert_eq!(affordance, InteractionAffordance::default());
    }

    #[test]
    fn full_interaction() {
        let affordance: UncheckedInteractionAffordance<Nil> =
            InteractionAffordanceBuilder::<Nil, ()>::default()
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
                .uri_variable("uri1", |b| b.finish_extend().number())
                .uri_variable("uri2", |b| b.finish_extend().integer())
                .into();
        let affordance: InteractionAffordance<Nil> = affordance.try_into().unwrap();

        assert_eq!(
            affordance,
            InteractionAffordance {
                attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
                title: Some("title".to_string()),
                titles: Some(
                    [("it", "title_it"), ("en", "title_en"),]
                        .into_iter()
                        .map(|(k, v)| (k.parse().unwrap(), v.to_string()))
                        .collect()
                ),
                description: Some("description".to_string()),
                descriptions: Some(
                    [("it", "description_it"), ("en", "description_en"),]
                        .into_iter()
                        .map(|(k, v)| (k.parse().unwrap(), v.to_string()))
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
        let affordance_builder: UsablePropertyAffordanceBuilder<Nil> = PropertyAffordanceBuilder::<
            Nil,
            PartialDataSchemaBuilder<_, _, _, _>,
            (),
            (),
        >::default()
        .title("property")
        .default_value(["hello", "world"].as_slice())
        .number()
        .observable(true)
        .form(|b| b.href("href"))
        .unit("cm")
        .read_only()
        .minimum(0.)
        .uri_variable("test", |b| b.finish_extend().bool())
        .into_usable();

        let affordance: PropertyAffordance<Nil> = affordance_builder.build().unwrap();

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
                    default: Some(json! { ["hello", "world"] }),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(Minimum::Inclusive(0.)),
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
        let affordance_builder: UsablePropertyAffordanceBuilder<Nil> = PropertyAffordanceBuilder::<
            Nil,
            PartialDataSchemaBuilder<_, _, _, _>,
            (),
            (),
        >::default()
        .title("property")
        .enumeration("enum1")
        .write_only()
        .enumeration("enum2")
        .observable(true)
        .form(|b| b.href("href"))
        .unit("cm")
        .into_usable();

        let affordance: PropertyAffordance<Nil> = affordance_builder.build().unwrap();

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
        let affordance_builder: UsablePropertyAffordanceBuilder<Nil> = PropertyAffordanceBuilder::<
            Nil,
            PartialDataSchemaBuilder<_, _, _, _>,
            (),
            (),
        >::default()
        .title("property")
        .one_of(|b| b.finish_extend().number())
        .one_of(|b| b.finish_extend().integer())
        .observable(true)
        .form(|b| b.href("href"))
        .unit("cm")
        .into_usable();

        let affordance: PropertyAffordance<Nil> = affordance_builder.build().unwrap();

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
                .input(|b| {
                    b.finish_extend()
                        .number()
                        .unit("cm")
                        .read_only()
                        .minimum(0.)
                })
                .form(|b| b.href("href"))
                .into_usable();

        let affordance: ActionAffordance<Nil> = affordance_builder.build().unwrap();

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
                        minimum: Some(Minimum::Inclusive(0.)),
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
                .input(|b| {
                    b.finish_extend()
                        .number()
                        .unit("cm")
                        .read_only()
                        .minimum(0.)
                })
                .idempotent()
                .output(|b| {
                    b.finish_extend()
                        .number()
                        .unit("cm")
                        .read_only()
                        .minimum(0.)
                })
                .form(|b| b.href("href"))
                .synchronous(true)
                .into_usable();

        let affordance: ActionAffordance<Nil> = affordance_builder.build().unwrap();

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
                        minimum: Some(Minimum::Inclusive(0.)),
                        ..Default::default()
                    })),
                    ..Default::default()
                }),
                output: Some(DataSchemaFromOther::<Nil> {
                    unit: Some("cm".to_owned()),
                    read_only: true,
                    subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                        minimum: Some(Minimum::Inclusive(0.)),
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
            EventAffordanceBuilder::<Nil, (), ()>::default()
                .title("event")
                .data(|b| {
                    b.finish_extend()
                        .number()
                        .unit("cm")
                        .read_only()
                        .minimum(0.)
                })
                .form(|b| b.href("href"))
                .into_usable();

        let affordance: EventAffordance<Nil> = affordance_builder.build().unwrap();

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
                        minimum: Some(Minimum::Inclusive(0.)),
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
            EventAffordanceBuilder::<Nil, (), ()>::default()
                .title("event")
                .cancellation(|b| b.finish_extend().integer())
                .data(|b| {
                    b.finish_extend()
                        .number()
                        .unit("cm")
                        .read_only()
                        .minimum(0.)
                })
                .subscription(|b| b.finish_extend().bool())
                .data_response(|b| b.finish_extend().string())
                .form(|b| b.href("href"))
                .into_usable();

        let affordance: EventAffordance<Nil> = affordance_builder.build().unwrap();

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
                        minimum: Some(Minimum::Inclusive(0.)),
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

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct A(i32);
    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct B(String);

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ThingExtA {}

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct InteractionAffordanceExtA {
        a: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct PropertyAffordanceExtA {
        b: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ActionAffordanceExtA {
        b: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct EventAffordanceExtA {
        c: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct FormExtA {
        d: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ExpectedResponseExtA {
        e: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct DataSchemaExtA {
        f: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ThingExtB {}

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct InteractionAffordanceExtB {
        g: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct PropertyAffordanceExtB {
        h: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ActionAffordanceExtB {
        i: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct EventAffordanceExtB {
        j: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct FormExtB {
        k: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ExpectedResponseExtB {
        l: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct DataSchemaExtB {
        m: B,
    }

    impl ExtendableThing for ThingExtA {
        type InteractionAffordance = InteractionAffordanceExtA;
        type PropertyAffordance = PropertyAffordanceExtA;
        type ActionAffordance = ActionAffordanceExtA;
        type EventAffordance = EventAffordanceExtA;
        type Form = FormExtA;
        type ExpectedResponse = ExpectedResponseExtA;
        type DataSchema = DataSchemaExtA;
        type ObjectSchema = ();
        type ArraySchema = ();
    }

    impl ExtendableThing for ThingExtB {
        type InteractionAffordance = InteractionAffordanceExtB;
        type PropertyAffordance = PropertyAffordanceExtB;
        type ActionAffordance = ActionAffordanceExtB;
        type EventAffordance = EventAffordanceExtB;
        type Form = FormExtB;
        type ExpectedResponse = ExpectedResponseExtB;
        type DataSchema = DataSchemaExtB;
        type ObjectSchema = ();
        type ArraySchema = ();
    }

    #[test]
    fn extend_interaction() {
        let affordance: UncheckedInteractionAffordance<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            InteractionAffordanceBuilder::<Cons<ThingExtB, Cons<ThingExtA, Nil>>, _>::empty()
                .title("title")
                .ext(InteractionAffordanceExtA { a: A(1) })
                .uri_variable("x", |b| {
                    b.ext(DataSchemaExtA { f: A(2) })
                        .ext_with(|| DataSchemaExtB {
                            m: B("a".to_string()),
                        })
                        .finish_extend()
                        .null()
                })
                .ext_with(|| InteractionAffordanceExtB {
                    g: B("b".to_string()),
                })
                .form(|b| {
                    b.ext(FormExtA { d: A(3) })
                        .ext(FormExtB {
                            k: B("c".to_string()),
                        })
                        .href("href")
                })
                .into();
        let affordance: InteractionAffordance<_> = affordance.try_into().unwrap();

        assert_eq!(
            affordance,
            InteractionAffordance {
                title: Some("title".to_string()),
                uri_variables: Some(
                    [(
                        "x".to_string(),
                        DataSchema {
                            subtype: Some(DataSchemaSubtype::Null),
                            other: Nil::cons(DataSchemaExtA { f: A(2) }).cons(DataSchemaExtB {
                                m: B("a".to_string())
                            }),
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
                forms: vec![Form {
                    href: "href".to_string(),
                    other: Nil::cons(FormExtA { d: A(3) }).cons(FormExtB {
                        k: B("c".to_string())
                    }),
                    op: Default::default(),
                    content_type: Default::default(),
                    content_coding: Default::default(),
                    subprotocol: Default::default(),
                    security: Default::default(),
                    scopes: Default::default(),
                    response: Default::default(),
                    additional_responses: Default::default(),
                }],
                other: Nil::cons(InteractionAffordanceExtA { a: A(1) }).cons(
                    InteractionAffordanceExtB {
                        g: B("b".to_string())
                    }
                ),
                attype: Default::default(),
                titles: Default::default(),
                description: Default::default(),
                descriptions: Default::default(),
            },
        );
    }

    #[test]
    fn extend_property() {
        let builder: UsablePropertyAffordanceBuilder<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            PropertyAffordanceBuilder::<Cons<ThingExtB, Cons<ThingExtA, Nil>>, _, _, _>::empty()
                .ext(PropertyAffordanceExtA { b: A(1) })
                .title("title")
                .ext_interaction(InteractionAffordanceExtA { a: A(2) })
                .ext_data_schema(DataSchemaExtA { f: A(4) })
                .uri_variable("x", |b| {
                    b.ext(DataSchemaExtA { f: A(3) })
                        .ext_with(|| DataSchemaExtB {
                            m: B("a".to_string()),
                        })
                        .finish_extend()
                        .null()
                })
                .ext_data_schema_with(|| DataSchemaExtB {
                    m: B("d".to_string()),
                })
                .ext_interaction_with(|| InteractionAffordanceExtB {
                    g: B("b".to_string()),
                })
                .finish_extend_data_schema()
                .ext_with(|| PropertyAffordanceExtB {
                    h: B("c".to_string()),
                })
                .null()
                .into_usable();
        let affordance: PropertyAffordance<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            builder.build().unwrap();

        assert_eq!(
            affordance,
            PropertyAffordance {
                interaction: InteractionAffordance {
                    other: Nil::cons(InteractionAffordanceExtA { a: A(2) }).cons(
                        InteractionAffordanceExtB {
                            g: B("b".to_string())
                        }
                    ),
                    uri_variables: Some(
                        [(
                            "x".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Null),
                                other: Nil::cons(DataSchemaExtA { f: A(3) }).cons(DataSchemaExtB {
                                    m: B("a".to_string())
                                }),
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
                    title: Some("title".to_string()),
                    titles: Default::default(),
                    description: Default::default(),
                    descriptions: Default::default(),
                    forms: Default::default(),
                },
                data_schema: DataSchema {
                    title: Some("title".to_string()),
                    subtype: Some(DataSchemaSubtype::Null),
                    other: Nil::cons(DataSchemaExtA { f: A(4) }).cons(DataSchemaExtB {
                        m: B("d".to_string())
                    }),
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
                },
                other: Nil::cons(PropertyAffordanceExtA { b: A(1) }).cons(PropertyAffordanceExtB {
                    h: B("c".to_string())
                }),
                observable: Default::default(),
            }
        );
    }

    #[test]
    fn extend_event() {
        let builder: UsableEventAffordanceBuilder<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            EventAffordanceBuilder::<Cons<ThingExtB, Cons<ThingExtA, Nil>>, _, _>::empty()
                .title("title")
                .ext_interaction(InteractionAffordanceExtA { a: A(1) })
                .uri_variable("x", |b| {
                    b.ext(DataSchemaExtA { f: A(2) })
                        .ext_with(|| DataSchemaExtB {
                            m: B("a".to_string()),
                        })
                        .finish_extend()
                        .null()
                })
                .ext_interaction_with(|| InteractionAffordanceExtB {
                    g: B("b".to_string()),
                })
                .ext(EventAffordanceExtA { c: A(3) })
                .ext_with(|| EventAffordanceExtB {
                    j: B("c".to_string()),
                })
                .subscription(|b| {
                    b.ext(DataSchemaExtA { f: A(4) })
                        .ext_with(|| DataSchemaExtB {
                            m: B("d".to_string()),
                        })
                        .finish_extend()
                        .null()
                })
                .into_usable();
        let affordance: EventAffordance<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            builder.build().unwrap();

        assert_eq!(
            affordance,
            EventAffordance {
                interaction: InteractionAffordance {
                    title: Some("title".to_string()),
                    uri_variables: Some(
                        [(
                            "x".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Null),
                                other: Nil::cons(DataSchemaExtA { f: A(2) }).cons(DataSchemaExtB {
                                    m: B("a".to_string())
                                }),
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
                    other: Nil::cons(InteractionAffordanceExtA { a: A(1) }).cons(
                        InteractionAffordanceExtB {
                            g: B("b".to_string())
                        }
                    ),
                    attype: Default::default(),
                    titles: Default::default(),
                    description: Default::default(),
                    descriptions: Default::default(),
                    forms: Default::default(),
                },
                subscription: Some(DataSchema {
                    subtype: Some(DataSchemaSubtype::Null),
                    other: Nil::cons(DataSchemaExtA { f: A(4) }).cons(DataSchemaExtB {
                        m: B("d".to_string())
                    }),
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
                other: Nil::cons(EventAffordanceExtA { c: A(3) }).cons(EventAffordanceExtB {
                    j: B("c".to_string())
                }),
                data: Default::default(),
                data_response: Default::default(),
                cancellation: Default::default(),
            },
        );
    }

    #[test]
    fn extend_action() {
        let builder: UsableActionAffordanceBuilder<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            ActionAffordanceBuilder::<Cons<ThingExtB, Cons<ThingExtA, Nil>>, _, _>::empty()
                .title("title")
                .ext_interaction(InteractionAffordanceExtA { a: A(1) })
                .uri_variable("x", |b| {
                    b.ext(DataSchemaExtA { f: A(2) })
                        .ext_with(|| DataSchemaExtB {
                            m: B("a".to_string()),
                        })
                        .finish_extend()
                        .null()
                })
                .ext_interaction_with(|| InteractionAffordanceExtB {
                    g: B("b".to_string()),
                })
                .ext(ActionAffordanceExtA { b: A(3) })
                .ext_with(|| ActionAffordanceExtB {
                    i: B("c".to_string()),
                })
                .input(|b| {
                    b.ext(DataSchemaExtA { f: A(4) })
                        .ext_with(|| DataSchemaExtB {
                            m: B("d".to_string()),
                        })
                        .finish_extend()
                        .null()
                })
                .into_usable();
        let affordance: ActionAffordance<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            builder.build().unwrap();

        assert_eq!(
            affordance,
            ActionAffordance {
                interaction: InteractionAffordance {
                    title: Some("title".to_string()),
                    uri_variables: Some(
                        [(
                            "x".to_string(),
                            DataSchema {
                                subtype: Some(DataSchemaSubtype::Null),
                                other: Nil::cons(DataSchemaExtA { f: A(2) }).cons(DataSchemaExtB {
                                    m: B("a".to_string())
                                }),
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
                    other: Nil::cons(InteractionAffordanceExtA { a: A(1) }).cons(
                        InteractionAffordanceExtB {
                            g: B("b".to_string())
                        }
                    ),
                    attype: Default::default(),
                    titles: Default::default(),
                    description: Default::default(),
                    descriptions: Default::default(),
                    forms: Default::default(),
                },
                input: Some(DataSchema {
                    subtype: Some(DataSchemaSubtype::Null),
                    other: Nil::cons(DataSchemaExtA { f: A(4) }).cons(DataSchemaExtB {
                        m: B("d".to_string())
                    }),
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
                other: Nil::cons(ActionAffordanceExtA { b: A(3) }).cons(ActionAffordanceExtB {
                    i: B("c".to_string())
                }),
                output: Default::default(),
                safe: Default::default(),
                idempotent: Default::default(),
                synchronous: Default::default(),
            },
        );
    }

    #[test]
    fn build_invalid_property_affordance() {
        let builder = PropertyAffordanceBuilder::<
            Nil,
            PartialDataSchemaBuilder<_, _, _, _>,
            (),
            (),
        >::default()
        .number()
        .titles(|b| b.add("i1t", "title1"))
        .into_usable();

        assert_eq!(
            builder.build().unwrap_err(),
            Error::InvalidLanguageTag("i1t".to_string()),
        );
    }

    #[test]
    fn build_invalid_action_affordance() {
        let builder = ActionAffordanceBuilder::<Nil, (), ()>::default()
            .titles(|b| b.add("i1t", "title1"))
            .into_usable();

        assert_eq!(
            builder.build().unwrap_err(),
            Error::InvalidLanguageTag("i1t".to_string()),
        );
    }

    #[test]
    fn build_invalid_event_affordance() {
        let builder = EventAffordanceBuilder::<Nil, (), ()>::default()
            .titles(|b| b.add("i1t", "title1"))
            .into_usable();

        assert_eq!(
            builder.build().unwrap_err(),
            Error::InvalidLanguageTag("i1t".to_string()),
        );
    }
}
