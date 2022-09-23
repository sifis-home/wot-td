//! The builder elements related to data schemas.
//!
use std::{cmp::Ordering, collections::HashMap, marker::PhantomData, ops::Not};

use crate::{
    extend::{Extend, Extendable, ExtendableThing},
    thing::{
        ArraySchema, DataSchema, DataSchemaSubtype, IntegerSchema, Maximum, Minimum, NumberSchema,
        ObjectSchema, StringSchema, UncheckedArraySchema, UncheckedDataSchemaSubtype,
        UncheckedObjectSchema,
    },
};

use super::{
    human_readable_info::{
        impl_delegate_buildable_hr_info, BuildableHumanReadableInfo, HumanReadableInfo,
    },
    Error, Extended, MultiLanguageBuilder, ToExtend,
};

/// The _unchecked_ variant of a [`DataSchema`](crate::thing::DataSchema).
///
/// This can be transformed into a valid `DataSchema` by
/// [`ThingBuilder::build`](crate::builder::ThingBuilder::build).
#[derive(Clone, Debug, Default, PartialEq)]
pub struct UncheckedDataSchema<DS, AS, OS> {
    attype: Option<Vec<String>>,
    title: Option<String>,
    titles: Option<MultiLanguageBuilder<String>>,
    description: Option<String>,
    descriptions: Option<MultiLanguageBuilder<String>>,
    constant: Option<Value>,
    default: Option<Value>,
    unit: Option<String>,
    one_of: Option<Vec<Self>>,
    enumeration: Option<Vec<Value>>,
    read_only: bool,
    write_only: bool,
    format: Option<String>,
    subtype: Option<UncheckedDataSchemaSubtype<DS, AS, OS>>,
    other: DS,
}

pub(crate) type UncheckedDataSchemaFromOther<Other> = UncheckedDataSchema<
    <Other as ExtendableThing>::DataSchema,
    <Other as ExtendableThing>::ArraySchema,
    <Other as ExtendableThing>::ObjectSchema,
>;

pub(crate) type UncheckedDataSchemaMap<Other> = HashMap<
    String,
    UncheckedDataSchema<
        <Other as ExtendableThing>::DataSchema,
        <Other as ExtendableThing>::ArraySchema,
        <Other as ExtendableThing>::ObjectSchema,
    >,
>;

/// _Partial_ variant of a [`DataSchemaBuilder`].
///
/// This variant is necessary for building a
/// [`PropertyAffordance`](crate::thing::PropertyAffordance), which is composed of a set of _human
/// readable_ fields shared between [`InteractionAffordance`](crate::thing::InteractionAffordance)
/// and [`DataSchema`].
#[derive(Debug, PartialEq)]
pub struct PartialDataSchemaBuilder<DS, AS, OS, Status> {
    constant: Option<Value>,
    default: Option<Value>,
    unit: Option<String>,
    one_of: Vec<UncheckedDataSchema<DS, AS, OS>>,
    enumeration: Vec<Value>,
    read_only: bool,
    write_only: bool,
    format: Option<String>,

    /// Data schema extension.
    pub other: DS,
    _marker: PhantomData<Status>,
}

impl<DS, AS, OS> PartialDataSchemaBuilder<DS, AS, OS, ToExtend> {
    pub(crate) fn empty() -> PartialDataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>
    where
        DS: Extendable,
    {
        PartialDataSchemaBuilder {
            constant: Default::default(),
            default: Default::default(),
            unit: Default::default(),
            one_of: Default::default(),
            enumeration: Default::default(),
            read_only: Default::default(),
            write_only: Default::default(),
            format: Default::default(),
            other: DS::empty(),
            _marker: PhantomData,
        }
    }
}

impl<DS, AS, OS> PartialDataSchemaBuilder<DS, AS, OS, ToExtend> {
    /// Extends the data schema, passing a closure that returns `T`.
    pub fn ext_with<F, T>(self, f: F) -> PartialDataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        F: FnOnce() -> T,
        DS: Extend<T>,
    {
        let Self {
            constant,
            default,
            unit,
            one_of: _,
            enumeration,
            read_only,
            write_only,
            format,
            other,
            _marker,
        } = self;
        let other = other.ext_with(f);
        PartialDataSchemaBuilder {
            constant,
            default,
            unit,
            one_of: Default::default(),
            enumeration,
            read_only,
            write_only,
            format,
            other,
            _marker,
        }
    }

    /// Extends the data schema with an additional element.
    #[inline]
    pub fn ext<T>(self, t: T) -> PartialDataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        DS: Extend<T>,
    {
        self.ext_with(|| t)
    }

    /// Makes the builder unextendable and allows further customizations.
    pub fn finish_extend(self) -> PartialDataSchemaBuilder<DS, AS, OS, Extended> {
        let Self {
            constant,
            default,
            unit,
            one_of,
            enumeration,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = self;
        PartialDataSchemaBuilder {
            constant,
            default,
            unit,
            one_of,
            enumeration,
            read_only,
            write_only,
            format,
            other,
            _marker: PhantomData,
        }
    }
}

impl<DS, AS, OS> Default for PartialDataSchemaBuilder<DS, AS, OS, Extended>
where
    DS: Default,
{
    fn default() -> Self {
        Self {
            constant: Default::default(),
            default: Default::default(),
            unit: Default::default(),
            one_of: Default::default(),
            enumeration: Default::default(),
            read_only: Default::default(),
            write_only: Default::default(),
            format: Default::default(),
            other: Default::default(),
            _marker: Default::default(),
        }
    }
}

/// _Partial_ variant of a [`DataSchema`].
///
/// This variant does not include the _human readable_ fields.
#[derive(Debug, Default, PartialEq)]
pub struct PartialDataSchema<DS, AS, OS> {
    pub(super) constant: Option<Value>,
    pub(super) default: Option<Value>,
    pub(super) unit: Option<String>,
    pub(super) one_of: Option<Vec<UncheckedDataSchema<DS, AS, OS>>>,
    pub(super) enumeration: Option<Vec<Value>>,
    pub(super) read_only: bool,
    pub(super) write_only: bool,
    pub(super) format: Option<String>,
    pub(super) subtype: Option<UncheckedDataSchemaSubtype<DS, AS, OS>>,

    /// Data schema extension.
    pub other: DS,
}

/// Basic builder for [`DataSchema`].
///
/// # Example
///
/// ```compile_fail
/// # use wot_td::{
/// #   thing::DataSchema,
/// #   builder::DataSchemaBuilder,
/// # };
/// let data_schema: DataSchema = DataSchemaBuilder::default().into();
/// ```
#[derive(Debug, PartialEq)]
pub struct DataSchemaBuilder<DS, AS, OS, Status> {
    partial: PartialDataSchemaBuilder<DS, AS, OS, Status>,
    info: HumanReadableInfo,
}

impl<DS, AS, OS> DataSchemaBuilder<DS, AS, OS, ToExtend> {
    /// Extends the data schema, passing a closure that returns `T`.
    pub fn ext_with<F, T>(self, f: F) -> DataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        F: FnOnce() -> T,
        DS: Extend<T>,
    {
        let Self { partial, info } = self;
        let partial = partial.ext_with(f);
        DataSchemaBuilder { partial, info }
    }

    /// Extends the data schema with an additional element.
    #[inline]
    pub fn ext<T>(self, t: T) -> DataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        DS: Extend<T>,
    {
        self.ext_with(|| t)
    }

    /// Makes the builder unextendable and allows further customizations.
    pub fn finish_extend(self) -> DataSchemaBuilder<DS, AS, OS, Extended> {
        let Self { partial, info } = self;
        let partial = partial.finish_extend();
        DataSchemaBuilder { partial, info }
    }
}

impl<DS, AS, OS> DataSchemaBuilder<DS, AS, OS, ToExtend> {
    pub(crate) fn empty() -> DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>
    where
        DS: Extendable,
    {
        DataSchemaBuilder {
            partial: PartialDataSchemaBuilder::<DS, _, _, _>::empty(),
            info: Default::default(),
        }
    }
}

impl<DS, AS, OS> Default for DataSchemaBuilder<DS, AS, OS, Extended>
where
    DS: Default,
{
    fn default() -> Self {
        Self {
            partial: Default::default(),
            info: Default::default(),
        }
    }
}

trait IntoDataSchema: Into<Self::Target> {
    type Target: Sized;
}

/// An interface for a buildable version of a [`DataSchema`](crate::thing::DataSchema).
///
/// In order to model the specification, each type that can be created using a builder pattern and
/// that _behaves_ like an `DataSchema` should implement this trait.
///
/// # Notes
///
/// This trait *should not* be implemented directly, even if it is not sealed.
pub trait BuildableDataSchema<DS, AS, OS, Status>: Sized {
    /// Sets the value of the `unit` field.
    fn unit(self, value: impl Into<String>) -> Self;

    /// Sets the value of the `format` field.
    fn format(self, value: impl Into<String>) -> Self;

    /// Sets the value of the `default` field.
    fn default_value(self, value: impl Into<Value>) -> Self;
}

/// An interface for a _specializable_ version of a [`DataSchema`](crate::thing::DataSchema).
///
/// A meaningful `DataSchema` should always contain a valid
/// [`subtype`](crate::thing::DataSchema::subtype) field, unless `enumeration` or `one_of` fields
/// are used. This trait allows to safely transform an _unspecialized_ `DataSchema` into a
/// _specialized_ one.
///
/// # Notes
///
/// - This trait *should not* be implemented directly, even if it is not sealed.
/// - This is going to break in future releases in order to have less, more expressive code.
pub trait SpecializableDataSchema<DS, AS, OS>: BuildableDataSchema<DS, AS, OS, Extended> {
    /// A generic stateless specialized data schema builder.
    type Stateless: BuildableDataSchema<DS, AS, OS, Extended>;

    /// The _array_ specialization of the data schema builder.
    type Array: BuildableDataSchema<DS, AS, OS, Extended>;

    /// The _number_ specialization of the data schema builder.
    type Number: BuildableDataSchema<DS, AS, OS, Extended>;

    /// The _integer_ specialization of the data schema builder.
    type Integer: BuildableDataSchema<DS, AS, OS, Extended>;

    /// The _object_ specialization of the data schema builder.
    type Object: BuildableDataSchema<DS, AS, OS, Extended>;

    /// The _string_ specialization of the data schema builder.
    type String: BuildableDataSchema<DS, AS, OS, Extended>;

    /// The _constant_ specialization of the data schema builder.
    type Constant: BuildableDataSchema<DS, AS, OS, Extended>;

    /// Specialize the builder into an _array_ data schema builder, initializing the array
    /// extensions with default values.
    fn array(self) -> Self::Array
    where
        AS: Default;

    /// Specialize the builder into an _array_ data schema builder, passing a function to
    /// create the array extensions.
    fn array_ext<F>(self, f: F) -> Self::Array
    where
        F: FnOnce(AS::Empty) -> AS,
        AS: Extendable;

    /// Specialize the builder into a _boolean_ data schema builder.
    fn bool(self) -> Self::Stateless;

    /// Specialize the builder into a _number_ data schema builder.
    fn number(self) -> Self::Number;

    /// Specialize the builder into an _integer_ data schema builder.
    fn integer(self) -> Self::Integer;
    /// Specialize the builder into an _object_ data schema builder, initializing the object
    /// extensions with default values.
    fn object(self) -> Self::Object
    where
        OS: Default;

    /// Specialize the builder into an _object_ data schema builder, passing a function to create
    /// the object extensions.
    fn object_ext<F>(self, f: F) -> Self::Object
    where
        F: FnOnce(OS::Empty) -> OS,
        OS: Extendable;

    /// Specialize the builder into a _string_ data schema builder.
    fn string(self) -> Self::String;

    /// Specialize the builder into a _null_ data schema builder.
    fn null(self) -> Self::Stateless;

    /// Specialize the builder into a _constant_ data schema builder.
    fn constant(self, value: impl Into<Value>) -> Self::Constant;
}

/// An interface to specialize an _enumerable_ version of a
/// [`DataSchema`](crate::thing::DataSchema).
///
/// An _unspecialized_ data schema can be _specialized_ into an _enumerable_ data schema, which
/// then supports adding more variants to the enumeration. This trait allows this behavior, keeping
/// it separated from [`SpecializableDataSchema`] that is not implemented for _specialized_ data
/// schemas.
///
/// # Notes
///
/// - This trait *should not* be implemented directly, even if it is not sealed.
pub trait EnumerableDataSchema<DS, AS, OS, Extended>:
    BuildableDataSchema<DS, AS, OS, Extended>
{
    /// The _enumeration_ specialization of the data schema builder.
    type Target: BuildableDataSchema<DS, AS, OS, Extended>;

    /// Returns a _specialized_ enumeration data schema and adds a variant to the `enumeration`
    /// field. It can be implemented for specialized _enumeration_ data schemas.
    fn enumeration(self, value: impl Into<Value>) -> Self::Target;
}

/// An interface to specialize a _union_ version of a [`DataSchema`](crate::thing::DataSchema).
///
/// An _unspecialized_ data schema can be _specialized_ into an _union_ data schema, which then
/// supports adding more data schemas to the `one_of` fields. This trait allows this behavior,
/// keeping it separated from [`SpecializableDataSchema`] that is not implemented for _specialized_
/// data schemas.
///
/// # Notes
///
/// - This trait *should not* be implemented directly, even if it is not sealed.
pub trait UnionDataSchema<DS, AS, OS>: BuildableDataSchema<DS, AS, OS, Extended> {
    /// The _union_ specialization of the data schema builder.
    type Target: BuildableDataSchema<DS, AS, OS, Extended>;

    /// Returns a _specialized_ union data schema and adds a data schema to the `one_of` field. It
    /// can be implemented for specialized _one_of_ data schemas.
    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>;
}

/// An interface to specialize a _read-only_/_write-only_ version of a
/// [`DataSchema`](crate::thing::DataSchema).
///
/// Some specializations of `DataSchema` can be set as _read-only_ or _write-only_. When
/// implemented, this allows a safe abstraction over these situations, avoiding conflicting states
/// a compile-time.
///
/// # Notes
///
/// - This trait *should not* be implemented directly, even if it is not sealed.
pub trait ReadableWriteableDataSchema<DS, AS, OS, Extended>:
    BuildableDataSchema<DS, AS, OS, Extended>
{
    /// The _read-only_ variant of the data schema builder.
    type ReadOnly: BuildableDataSchema<DS, AS, OS, Extended>;

    /// The _write-only_ variant of the data schema builder.
    type WriteOnly: BuildableDataSchema<DS, AS, OS, Extended>;

    /// Creates a _read-only_ variant of the data schema builder.
    fn read_only(self) -> Self::ReadOnly;

    /// Creates a _write-only_ variant of the data schema builder.
    fn write_only(self) -> Self::WriteOnly;
}

/// The builder for an [`ArraySchema`](crate::thing::ArraySchema) builder.
pub struct ArrayDataSchemaBuilder<Inner, DS, AS, OS> {
    inner: Inner,
    items: Vec<UncheckedDataSchema<DS, AS, OS>>,
    min_items: Option<u32>,
    max_items: Option<u32>,

    /// Array data schema extension.
    pub other: AS,
}

/// The builder for an [`NumberSchema`](crate::thing::NumberSchema) builder.
pub struct NumberDataSchemaBuilder<Inner> {
    inner: Inner,
    maximum: Option<Maximum<f64>>,
    minimum: Option<Minimum<f64>>,
    multiple_of: Option<f64>,
}

/// The builder for an [`IntegerSchema`](crate::thing::IntegerSchema) builder.
pub struct IntegerDataSchemaBuilder<Inner> {
    inner: Inner,
    maximum: Option<Maximum<usize>>,
    minimum: Option<Minimum<usize>>,
}

/// The builder for an [`ObjectSchema`](crate::thing::ObjectSchema) builder.
pub struct ObjectDataSchemaBuilder<Inner, DS, AS, OS> {
    inner: Inner,
    properties: Vec<(String, UncheckedDataSchema<DS, AS, OS>)>,
    required: Vec<String>,

    /// Object data schema extension.
    pub other: OS,
}

/// The builder for an [`StringSchema`](crate::thing::StringSchema) builder.
pub struct StringDataSchemaBuilder<Inner> {
    inner: Inner,
    min_length: Option<u32>,
    max_length: Option<u32>,
    pattern: Option<String>,
    content_encoding: Option<String>,
    content_media_type: Option<String>,
}

/// A _typetag_ for a `DataSchema` builder that has the
/// [`enumeration`](crate::thing::DataSchema::enumeration) field populated.
pub struct EnumDataSchemaBuilder<Inner> {
    inner: Inner,
}

/// A _typetag_ for a `DataSchema` builder that has the
/// [`one_of`](crate::thing::DataSchema::one_of) field populated.
pub struct OneOfDataSchemaBuilder<Inner> {
    inner: Inner,
}

/// The type of a stateless `DataSchema` specialization.
pub enum StatelessDataSchemaType {
    /// A _boolean_ specialization.
    Boolean,

    /// A _null_ specialization.
    Null,
}

/// A _typetag_ for a stateless specialized `DataSchema` builder.
pub struct StatelessDataSchemaBuilder<Inner> {
    inner: Inner,
    ty: Option<StatelessDataSchemaType>,
}

/// A _typetag_ for a read-only `DataSchema` builder.
pub struct ReadOnly<Inner> {
    inner: Inner,
}

/// A _typetag_ for a write-only `DataSchema` builder.
pub struct WriteOnly<Inner> {
    inner: Inner,
}

macro_rules! opt_field_decl {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            #[doc = concat!("Sets the value of the `", stringify!($field), "` field.")]
            fn $field(self, value: $ty) -> Self;
        )*
    };
}

macro_rules! opt_field_into_decl {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            #[doc = concat!("Sets the value of the `", stringify!($field), "` field.")]
            fn $field(self, value: impl Into<$ty>) -> Self;
        )*
    };
}

/// An interface for things behaving like an array data schema builder.
pub trait ArrayDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(min_items: u32, max_items: u32);

    /// Append an element to the array.
    fn append<F, T>(self, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>;
}

/// An interface for things behaving like a number data schema builder.
pub trait NumberDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(
        minimum: f64,
        maximum: f64,
        exclusive_minimum: f64,
        exclusive_maximum: f64,
        multiple_of: f64,
    );
}

/// An interface for things behaving like an integer data schema builder.
pub trait IntegerDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(
        minimum: usize,
        maximum: usize,
        exclusive_minimum: usize,
        exclusive_maximum: usize,
    );
}

/// An interface for things behaving like an object data schema builder.
pub trait ObjectDataSchemaBuilderLike<DS, AS, OS> {
    /// Add a new property to the object.
    ///
    /// The `name` corresponds to the _key_ of the object.
    ///
    /// If `required` is true, the `name` is added to the
    /// [`required`](crate::thing::ObjectSchema::required) field.
    fn property<F, T>(self, name: impl Into<String>, required: bool, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>;
}

/// An interface for things behaving like a string data schema builder.
pub trait StringDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(min_length: u32, max_length: u32);

    opt_field_into_decl!(
        pattern: String,
        content_encoding: String,
        content_media_type: String,
    );
}

macro_rules! opt_field_builder {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            fn $field(mut self, value: $ty) -> Self {
                self.$field = Some(value);
                self
            }
        )*
    };
}

macro_rules! opt_field_into_builder {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            fn $field(mut self, value: impl Into<$ty>) -> Self {
                self.$field = Some(value.into());
                self
            }
        )*
    };
}

impl<Inner, DS, AS, OS> ArrayDataSchemaBuilderLike<DS, AS, OS>
    for ArrayDataSchemaBuilder<Inner, DS, AS, OS>
where
    Inner: BuildableDataSchema<DS, AS, OS, Extended>,
{
    opt_field_builder!(min_items: u32, max_items: u32);

    fn append<F, T>(mut self, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>,
    {
        self.items
            .push(f(DataSchemaBuilder::<DS, _, _, _>::empty()).into());
        self
    }
}

impl<Inner: BuildableDataSchema<DS, AS, OS, Extended>, DS, AS, OS>
    NumberDataSchemaBuilderLike<DS, AS, OS> for NumberDataSchemaBuilder<Inner>
{
    opt_field_builder!(multiple_of: f64);

    fn minimum(mut self, value: f64) -> Self {
        self.minimum = Some(Minimum::Inclusive(value));
        self
    }

    fn exclusive_minimum(mut self, value: f64) -> Self {
        self.minimum = Some(Minimum::Exclusive(value));
        self
    }

    fn maximum(mut self, value: f64) -> Self {
        self.maximum = Some(Maximum::Inclusive(value));
        self
    }

    fn exclusive_maximum(mut self, value: f64) -> Self {
        self.maximum = Some(Maximum::Exclusive(value));
        self
    }
}

impl<Inner: BuildableDataSchema<DS, AS, OS, Extended>, DS, AS, OS>
    IntegerDataSchemaBuilderLike<DS, AS, OS> for IntegerDataSchemaBuilder<Inner>
{
    fn minimum(mut self, value: usize) -> Self {
        self.minimum = Some(Minimum::Inclusive(value));
        self
    }

    fn exclusive_minimum(mut self, value: usize) -> Self {
        self.minimum = Some(Minimum::Exclusive(value));
        self
    }

    fn maximum(mut self, value: usize) -> Self {
        self.maximum = Some(Maximum::Inclusive(value));
        self
    }

    fn exclusive_maximum(mut self, value: usize) -> Self {
        self.maximum = Some(Maximum::Exclusive(value));
        self
    }
}

impl<Inner, DS, AS, OS> ObjectDataSchemaBuilderLike<DS, AS, OS>
    for ObjectDataSchemaBuilder<Inner, DS, AS, OS>
where
    Inner: BuildableDataSchema<DS, AS, OS, Extended>,
{
    fn property<F, T>(mut self, name: impl Into<String>, required: bool, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>,
    {
        let data_schema = f(DataSchemaBuilder::<DS, _, _, _>::empty()).into();
        let name = name.into();

        if required {
            self.required.push(name.clone());
        }

        self.properties.push((name, data_schema));
        self
    }
}

impl<Inner: BuildableDataSchema<DS, AS, OS, Extended>, DS, AS, OS>
    StringDataSchemaBuilderLike<DS, AS, OS> for StringDataSchemaBuilder<Inner>
{
    opt_field_builder!(min_length: u32, max_length: u32);

    opt_field_into_builder!(
        pattern: String,
        content_encoding: String,
        content_media_type: String,
    );
}

macro_rules! impl_inner_delegate_schema_builder_like_array {
    ($inner:ident) => {
        #[inline]
        fn min_items(mut self, value: u32) -> Self {
            self.$inner = self.$inner.min_items(value);
            self
        }

        #[inline]
        fn max_items(mut self, value: u32) -> Self {
            self.$inner = self.$inner.max_items(value);
            self
        }

        #[inline]
        fn append<F, T>(mut self, f: F) -> Self
        where
            F: FnOnce(
                crate::builder::data_schema::DataSchemaBuilder<
                    <DS as Extendable>::Empty,
                    AS,
                    OS,
                    crate::builder::ToExtend,
                >,
            ) -> T,
            DS: Extendable,
            T: Into<crate::builder::data_schema::UncheckedDataSchema<DS, AS, OS>>,
        {
            self.$inner = self.$inner.append(f);
            self
        }
    };
}

macro_rules! impl_inner_delegate_schema_builder_like_number {
    ($inner:ident) => {
        #[inline]
        fn minimum(mut self, value: f64) -> Self {
            self.$inner = self.$inner.minimum(value);
            self
        }

        #[inline]
        fn maximum(mut self, value: f64) -> Self {
            self.$inner = self.$inner.maximum(value);
            self
        }

        #[inline]
        fn exclusive_minimum(mut self, value: f64) -> Self {
            self.$inner = self.$inner.exclusive_minimum(value);
            self
        }

        #[inline]
        fn exclusive_maximum(mut self, value: f64) -> Self {
            self.$inner = self.$inner.exclusive_maximum(value);
            self
        }

        #[inline]
        fn multiple_of(mut self, value: f64) -> Self {
            self.$inner = self.$inner.multiple_of(value);
            self
        }
    };
}

macro_rules! impl_inner_delegate_schema_builder_like_integer {
    ($inner:ident) => {
        #[inline]
        fn minimum(mut self, value: usize) -> Self {
            self.$inner = self.$inner.minimum(value);
            self
        }

        #[inline]
        fn maximum(mut self, value: usize) -> Self {
            self.$inner = self.$inner.maximum(value);
            self
        }

        #[inline]
        fn exclusive_minimum(mut self, value: usize) -> Self {
            self.$inner = self.$inner.exclusive_minimum(value);
            self
        }

        #[inline]
        fn exclusive_maximum(mut self, value: usize) -> Self {
            self.$inner = self.$inner.exclusive_maximum(value);
            self
        }
    };
}

macro_rules! impl_inner_delegate_schema_builder_like_object {
    ($inner:ident) => {
        #[inline]
        fn property<F, T>(mut self, name: impl Into<String>, required: bool, f: F) -> Self
        where
            F: FnOnce(
                crate::builder::data_schema::DataSchemaBuilder<
                    <DS as Extendable>::Empty,
                    AS,
                    OS,
                    crate::builder::ToExtend,
                >,
            ) -> T,
            DS: Extendable,
            T: Into<crate::builder::data_schema::UncheckedDataSchema<DS, AS, OS>>,
        {
            self.$inner = self.$inner.property(name, required, f);
            self
        }
    };
}

macro_rules! impl_delegate_schema_builder_like {
    ($( $ty:ident <$( $generic:ident ),+> on $inner:ident ),+ $(,)?) => {
        $(
            impl<DS, AS, OS, $($generic: crate::builder::data_schema::ArrayDataSchemaBuilderLike<DS, AS, OS>),+ > crate::builder::data_schema::ArrayDataSchemaBuilderLike<DS, AS, OS> for $ty< $($generic),+ > {
                crate::builder::data_schema::impl_inner_delegate_schema_builder_like_array!($inner);
            }

            impl<DS, AS, OS, $($generic: crate::builder::data_schema::NumberDataSchemaBuilderLike<DS, AS, OS>),+ > crate::builder::data_schema::NumberDataSchemaBuilderLike<DS, AS, OS> for $ty< $($generic),+ > {
                crate::builder::data_schema::impl_inner_delegate_schema_builder_like_number!($inner);
            }

            impl<DS, AS, OS, $($generic: crate::builder::data_schema::IntegerDataSchemaBuilderLike<DS, AS, OS>),+ > crate::builder::data_schema::IntegerDataSchemaBuilderLike<DS, AS, OS> for $ty< $($generic),+ > {
                crate::builder::data_schema::impl_inner_delegate_schema_builder_like_integer!($inner);
            }

            impl<DS, AS, OS, $($generic: crate::builder::data_schema::ObjectDataSchemaBuilderLike<DS, AS, OS>),+ > crate::builder::data_schema::ObjectDataSchemaBuilderLike<DS, AS, OS> for $ty< $($generic),+ > {
                crate::builder::data_schema::impl_inner_delegate_schema_builder_like_object!($inner);
            }
        )+
    };
}
pub(super) use impl_delegate_schema_builder_like;
pub(super) use impl_inner_delegate_schema_builder_like_array;
pub(super) use impl_inner_delegate_schema_builder_like_integer;
pub(super) use impl_inner_delegate_schema_builder_like_number;
pub(super) use impl_inner_delegate_schema_builder_like_object;

impl_delegate_schema_builder_like!(ReadOnly<Inner> on inner, WriteOnly<Innner> on inner);

macro_rules! buildable_data_schema_delegate {
    ($self:ident . $field:ident -> $fn:ident($($arg:ident),*)) => {{
        $self.$field = $self.$field.$fn($($arg),*);
        $self
    }};
}

macro_rules! impl_delegate_buildable_data_schema {
    () => {};

    ($kind:ident <DS, AS, OS $(, $($ty:ident),+)?> : $inner:ident $(, $($rest:tt)*)?) => {
        impl <DS, AS, OS $(, $($ty),+)? > crate::builder::data_schema::BuildableDataSchema<DS, AS, OS, crate::builder::Extended> for $kind <$($($ty),+ ,)? DS, AS, OS>
        $(
            where
                $($ty: crate::builder::data_schema::BuildableDataSchema<DS, AS, OS, crate::builder::Extended>),+
        )?
        {
            #[inline]
            fn unit(mut self, value: impl Into<String>) -> Self {
                crate::builder::data_schema::buildable_data_schema_delegate!(self.$inner -> unit(value))
            }

            #[inline]
            fn format(mut self, value: impl Into<String>) -> Self {
                crate::builder::data_schema::buildable_data_schema_delegate!(self.$inner -> format(value))
            }

            #[inline]
            fn default_value(mut self, value: impl Into<Value>) -> Self {
                crate::builder::data_schema::buildable_data_schema_delegate!(self.$inner -> default_value(value))
            }
        }

        $(
            crate::builder::data_schema::impl_delegate_buildable_data_schema!($($rest)*);
        )?
    };

    ($kind:ident $(<$($ty:ident),+>)? : $inner:ident $(, $($rest:tt)*)?) => {
        impl <DS, AS, OS, $($($ty),+)? > crate::builder::data_schema::BuildableDataSchema<DS, AS, OS, crate::builder::Extended> for $kind $(<$($ty),+>)?
        $(
            where
                $($ty: crate::builder::data_schema::BuildableDataSchema<DS, AS, OS, crate::builder::Extended>),+
        )?
        {
            #[inline]
            fn unit(mut self, value: impl Into<String>) -> Self {
                crate::builder::data_schema::buildable_data_schema_delegate!(self.$inner -> unit(value))
            }

            #[inline]
            fn format(mut self, value: impl Into<String>) -> Self {
                crate::builder::data_schema::buildable_data_schema_delegate!(self.$inner -> format(value))
            }

            #[inline]
            fn default_value(mut self, value: impl Into<Value>) -> Self {
                crate::builder::data_schema::buildable_data_schema_delegate!(self.$inner -> default_value(value))
            }
        }

        $(
            crate::builder::data_schema::impl_delegate_buildable_data_schema!($($rest)*);
        )?
    };

    ($kind:ident $(<$($ty:ident),+>)? $(, $($rest:tt)*)? ) => {
        crate::builder::data_schema::impl_delegate_buildable_data_schema!($kind $(<$($ty),+>)?: inner $(, $($rest)*)?);
    };
}

impl_delegate_buildable_data_schema!(
    ArrayDataSchemaBuilder<DS, AS, OS, Inner>,
    NumberDataSchemaBuilder<Inner>,
    IntegerDataSchemaBuilder<Inner>,
    ObjectDataSchemaBuilder<DS, AS, OS, Inner>,
    StringDataSchemaBuilder<Inner>,
    StatelessDataSchemaBuilder<Inner>,
    ReadOnly<Inner>,
    WriteOnly<Inner>,
    EnumDataSchemaBuilder<Inner>,
    OneOfDataSchemaBuilder<Inner>,
);

impl<DS, AS, OS, Status> BuildableDataSchema<DS, AS, OS, Status>
    for DataSchemaBuilder<DS, AS, OS, Status>
{
    #[inline]
    fn unit(mut self, value: impl Into<String>) -> Self {
        buildable_data_schema_delegate!(self.partial -> unit(value))
    }

    #[inline]
    fn format(mut self, value: impl Into<String>) -> Self {
        buildable_data_schema_delegate!(self.partial-> format(value))
    }

    #[inline]
    fn default_value(mut self, value: impl Into<Value>) -> Self {
        buildable_data_schema_delegate!(self.partial -> default_value(value))
    }
}

pub(crate) use buildable_data_schema_delegate;
pub(crate) use impl_delegate_buildable_data_schema;
use serde_json::Value;

macro_rules! trait_opt_field_builder {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            fn $field(mut self, value: impl Into<$ty>) -> Self {
                self.$field = Some(value.into());
                self
            }
        )*
    };
}

impl_delegate_buildable_hr_info! (
    DataSchemaBuilder<DS, AS, OS, Status> on info,
);

impl<DS, AS, OS, Status> BuildableDataSchema<DS, AS, OS, Status>
    for PartialDataSchemaBuilder<DS, AS, OS, Status>
{
    trait_opt_field_builder!(unit: String, format: String);

    fn default_value(mut self, value: impl Into<Value>) -> Self {
        self.default = Some(value.into());
        self
    }
}

impl_delegate_buildable_hr_info!(
    ArrayDataSchemaBuilder<Inner: BuildableHumanReadableInfo, DS, AS, OS> on inner,
    NumberDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    IntegerDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    ObjectDataSchemaBuilder<Inner: BuildableHumanReadableInfo, DS, AS, OS> on inner,
    StringDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    EnumDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    OneOfDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    StatelessDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    ReadOnly<Inner: BuildableHumanReadableInfo> on inner,
    WriteOnly<Inner: BuildableHumanReadableInfo> on inner,
);

macro_rules! impl_specializable_data_schema {
    ($($ty:ty $( : $($inner_path:ident).+ )? ),+ $(,)?) => {
        $(
            impl<DS, AS, OS> SpecializableDataSchema<DS, AS, OS> for $ty {
                type Stateless = StatelessDataSchemaBuilder<Self>;
                type Array = ArrayDataSchemaBuilder<Self, DS, AS, OS>;
                type Number = NumberDataSchemaBuilder<Self>;
                type Integer = IntegerDataSchemaBuilder<Self>;
                type Object = ObjectDataSchemaBuilder<Self, DS, AS, OS>;
                type String = StringDataSchemaBuilder<Self>;
                type Constant = ReadOnly<StatelessDataSchemaBuilder<Self>>;

                fn array(self) -> Self::Array
                where
                    AS: Default
                {
                    ArrayDataSchemaBuilder {
                        inner: self,
                        items: Default::default(),
                        min_items: Default::default(),
                        max_items: Default::default(),
                        other: Default::default(),
                    }
                }

                fn array_ext<F>(self, f: F) -> Self::Array
                where
                    F: FnOnce(AS::Empty) -> AS,
                    AS: Extendable,
                {
                    let other = f(AS::empty());

                    ArrayDataSchemaBuilder {
                        inner: self,
                        items: Default::default(),
                        min_items: Default::default(),
                        max_items: Default::default(),
                        other,
                    }
                }

                fn bool(self) -> Self::Stateless {
                    StatelessDataSchemaBuilder {
                        inner: self,
                        ty: Some(StatelessDataSchemaType::Boolean),
                    }
                }

                fn number(self) -> Self::Number {
                    NumberDataSchemaBuilder {
                        inner: self,
                        maximum: Default::default(),
                        minimum: Default::default(),
                        multiple_of: Default::default(),
                    }
                }

                fn integer(self) -> Self::Integer {
                    IntegerDataSchemaBuilder {
                        inner: self,
                        maximum: Default::default(),
                        minimum: Default::default(),
                    }
                }

                fn object(self) -> Self::Object
                where
                    OS: Default
                {
                    ObjectDataSchemaBuilder {
                        inner: self,
                        properties: Default::default(),
                        required: Default::default(),
                        other: Default::default(),
                    }
                }

                fn object_ext<F>(self, f: F) -> Self::Object
                where
                    F: FnOnce(OS::Empty) -> OS,
                    OS: Extendable,
                {
                    let other = f(OS::empty());

                    ObjectDataSchemaBuilder {
                        inner: self,
                        properties: Default::default(),
                        required: Default::default(),
                        other,
                    }
                }

                fn string(self) -> Self::String {
                    StringDataSchemaBuilder {
                        inner: self,
                        min_length: Default::default(),
                        max_length: Default::default(),
                        pattern: Default::default(),
                        content_encoding: Default::default(),
                        content_media_type: Default::default(),
                    }
                }

                fn null(self) -> Self::Stateless {
                    StatelessDataSchemaBuilder {
                        inner: self,
                        ty: Some(StatelessDataSchemaType::Null),
                    }
                }

                fn constant(mut self, value: impl Into<Value>) -> Self::Constant {
                    self $(. $($inner_path).+)?.constant = Some(value.into());
                    ReadOnly {
                        inner: StatelessDataSchemaBuilder {
                            inner: self,
                            ty: None,
                        },
                    }
                }
            }
        )+
    };
}

impl_specializable_data_schema!(PartialDataSchemaBuilder<DS, AS, OS, Extended>, DataSchemaBuilder<DS, AS, OS, Extended>: partial);

macro_rules! impl_enumerable_data_schema {
    ($($ty:ty $( : $($inner_path:ident).+ )? ),+ $(,)?) => {
        $(
        impl<DS, AS, OS> EnumerableDataSchema<DS, AS, OS, Extended> for $ty {
            type Target = EnumDataSchemaBuilder<Self>;

            fn enumeration(mut self, value: impl Into<Value>) -> EnumDataSchemaBuilder<Self> {
                self $(. $($inner_path).+ )?.enumeration.push(value.into());
                EnumDataSchemaBuilder { inner: self }
            }
        }
        )+
    };
}

impl_enumerable_data_schema!(PartialDataSchemaBuilder<DS, AS, OS, Extended>, DataSchemaBuilder<DS, AS, OS, Extended>: partial);

impl<Inner, DS, AS, OS> EnumerableDataSchema<DS, AS, OS, Extended> for ReadOnly<Inner>
where
    Inner: EnumerableDataSchema<DS, AS, OS, Extended>,
{
    type Target = ReadOnly<Inner::Target>;

    #[inline]
    fn enumeration(self, value: impl Into<Value>) -> Self::Target {
        let Self { inner } = self;

        let inner = inner.enumeration(value);
        ReadOnly { inner }
    }
}

impl<Inner, DS, AS, OS> EnumerableDataSchema<DS, AS, OS, Extended> for WriteOnly<Inner>
where
    Inner: EnumerableDataSchema<DS, AS, OS, Extended>,
{
    type Target = WriteOnly<Inner::Target>;

    #[inline]
    fn enumeration(self, value: impl Into<Value>) -> Self::Target {
        let Self { inner } = self;

        let inner = inner.enumeration(value);
        WriteOnly { inner }
    }
}

impl<DS, AS, OS> EnumerableDataSchema<DS, AS, OS, Extended>
    for EnumDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>>
{
    type Target = Self;

    #[inline]
    fn enumeration(mut self, value: impl Into<Value>) -> Self::Target {
        self.inner.enumeration.push(value.into());
        self
    }
}

impl<DS, AS, OS> EnumerableDataSchema<DS, AS, OS, Extended>
    for EnumDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>>
{
    type Target = Self;

    #[inline]
    fn enumeration(mut self, value: impl Into<Value>) -> Self::Target {
        self.inner.partial.enumeration.push(value.into());
        self
    }
}

macro_rules! impl_union_data_schema {
    ($($ty:ty $( : $($inner_path:ident).+ )? ),+ $(,)?) => {
        $(
            impl<DS, AS, OS> UnionDataSchema<DS, AS, OS> for $ty
            {
                type Target = OneOfDataSchemaBuilder<Self>;

                fn one_of<F, T>(mut self, f: F) -> Self::Target
                where
                    F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
                    DS: Extendable,
                    T: Into<UncheckedDataSchema<DS, AS, OS>>,
                {
                    self $(. $($inner_path).+ )? .one_of.push(f(DataSchemaBuilder::<DS, _, _, _>::empty()).into());
                    OneOfDataSchemaBuilder { inner: self }
                }
            }
        )+
    };
}

impl_union_data_schema!(PartialDataSchemaBuilder<DS, AS, OS, Extended>, DataSchemaBuilder<DS, AS, OS, Extended>: partial);

impl<Inner, DS, AS, OS> UnionDataSchema<DS, AS, OS> for ReadOnly<Inner>
where
    Inner: UnionDataSchema<DS, AS, OS>,
{
    type Target = ReadOnly<Inner::Target>;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>,
    {
        let Self { inner } = self;
        let inner = inner.one_of(f);
        ReadOnly { inner }
    }
}

impl<Inner, DS, AS, OS> UnionDataSchema<DS, AS, OS> for WriteOnly<Inner>
where
    Inner: UnionDataSchema<DS, AS, OS>,
{
    type Target = WriteOnly<Inner::Target>;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>,
    {
        let Self { inner } = self;
        let inner = inner.one_of(f);
        WriteOnly { inner }
    }
}

impl<DS, AS, OS> UnionDataSchema<DS, AS, OS>
    for OneOfDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>>
{
    type Target = Self;

    fn one_of<F, T>(mut self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>,
    {
        self.inner
            .one_of
            .push(f(DataSchemaBuilder::<DS, _, _, _>::empty()).into());
        self
    }
}

impl<DS, AS, OS> UnionDataSchema<DS, AS, OS>
    for OneOfDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>>
{
    type Target = Self;

    fn one_of<F, T>(mut self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<UncheckedDataSchema<DS, AS, OS>>,
    {
        self.inner
            .partial
            .one_of
            .push(f(DataSchemaBuilder::<DS, _, _, _>::empty()).into());
        self
    }
}

macro_rules! impl_rw_data_schema {
    ($( $ty:ty; $($inner_path:ident).+ ),+ $(,)?) => {
        $(
            impl<DS, AS, OS> ReadableWriteableDataSchema<DS, AS, OS, Extended> for $ty
            {
                type ReadOnly = ReadOnly<Self>;
                type WriteOnly = WriteOnly<Self>;

                #[inline]
                fn read_only(mut self) -> Self::ReadOnly {
                    self.$($inner_path).+.read_only = true;
                    ReadOnly {
                        inner: self,
                    }
                }

                #[inline]
                fn write_only(mut self) -> Self::WriteOnly {
                    self.$($inner_path).+.write_only = true;
                    WriteOnly {
                        inner: self,
                    }
                }
            }
        )+
    };
}

impl_rw_data_schema!(
    StatelessDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>>; inner.partial,
    StatelessDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>>; inner,
    ArrayDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>, DS, AS, OS>; inner.partial,
    ArrayDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>, DS, AS, OS>; inner,
    NumberDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>>; inner.partial,
    NumberDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>>; inner,
    IntegerDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>>; inner.partial,
    IntegerDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>>; inner,
    ObjectDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>, DS, AS, OS>; inner.partial,
    ObjectDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>, DS, AS, OS>; inner,
    StringDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>>; inner.partial,
    StringDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>>; inner,
    EnumDataSchemaBuilder<DataSchemaBuilder<DS, AS, OS, Extended>>; inner.partial,
    EnumDataSchemaBuilder<PartialDataSchemaBuilder<DS, AS, OS, Extended>>; inner,
);

impl<T, DS, AS, OS> From<ReadOnly<T>> for DataSchemaBuilder<DS, AS, OS, Extended>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(data_schema: ReadOnly<T>) -> Self {
        let DataSchemaBuilder { mut partial, info } = data_schema.inner.into();
        partial.read_only = true;

        Self { partial, info }
    }
}

impl<T, DS, AS, OS> From<WriteOnly<T>> for DataSchemaBuilder<DS, AS, OS, Extended>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(data_schema: WriteOnly<T>) -> Self {
        let DataSchemaBuilder { mut partial, info } = data_schema.inner.into();
        partial.write_only = true;

        Self { partial, info }
    }
}

impl<T, DS, AS, OS> From<ReadOnly<T>> for PartialDataSchemaBuilder<DS, AS, OS, Extended>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(data_schema: ReadOnly<T>) -> Self {
        let mut data_schema = data_schema.inner.into();
        data_schema.read_only = true;
        data_schema
    }
}

impl<T, DS, AS, OS> From<WriteOnly<T>> for PartialDataSchemaBuilder<DS, AS, OS, Extended>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(data_schema: WriteOnly<T>) -> Self {
        let mut data_schema = data_schema.inner.into();
        data_schema.write_only = true;
        data_schema
    }
}

impl<DS, AS, OS> From<StatelessDataSchemaType> for UncheckedDataSchemaSubtype<DS, AS, OS> {
    fn from(ty: StatelessDataSchemaType) -> Self {
        match ty {
            StatelessDataSchemaType::Boolean => UncheckedDataSchemaSubtype::Boolean,
            StatelessDataSchemaType::Null => UncheckedDataSchemaSubtype::Null,
        }
    }
}

impl<T, DS, AS, OS> From<StatelessDataSchemaBuilder<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StatelessDataSchemaBuilder<T>) -> Self {
        let StatelessDataSchemaBuilder { inner, ty } = builder;
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant,
                    default,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                    other,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = inner.into();

        let subtype = ty.map(Into::into);

        UncheckedDataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<StatelessDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: StatelessDataSchemaBuilder<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<StatelessDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StatelessDataSchemaBuilder<T>) -> Self {
        let StatelessDataSchemaBuilder { inner, ty } = builder;
        let PartialDataSchemaBuilder {
            constant,
            default,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = inner.into();

        let subtype = ty.map(Into::into);

        PartialDataSchema {
            constant,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> From<ArrayDataSchemaBuilder<T, DS, AS, OS>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: ArrayDataSchemaBuilder<T, DS, AS, OS>) -> Self {
        let ArrayDataSchemaBuilder {
            inner,
            items,
            min_items,
            max_items,
            other: other_array_schema,
        } = builder;
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
                    default,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                    other: other_data_schema,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = inner.into();

        let items = items.is_empty().not().then_some(items);
        let subtype = Some(UncheckedDataSchemaSubtype::Array(UncheckedArraySchema {
            items,
            min_items,
            max_items,
            other: other_array_schema,
        }));

        UncheckedDataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other: other_data_schema,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<ArrayDataSchemaBuilder<T, DS, AS, OS>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: ArrayDataSchemaBuilder<T, DS, AS, OS>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<ArrayDataSchemaBuilder<T, DS, AS, OS>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: ArrayDataSchemaBuilder<T, DS, AS, OS>) -> Self {
        let ArrayDataSchemaBuilder {
            inner,
            items,
            min_items,
            max_items,
            other: other_array_schema,
        } = builder;
        let PartialDataSchemaBuilder {
            constant: _,
            default,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other: other_data_schema,
            _marker: _,
        } = inner.into();

        let items = items.is_empty().not().then_some(items);
        let subtype = Some(UncheckedDataSchemaSubtype::Array(UncheckedArraySchema {
            items,
            min_items,
            max_items,
            other: other_array_schema,
        }));

        PartialDataSchema {
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other: other_data_schema,
        }
    }
}

impl<T, DS, AS, OS> From<NumberDataSchemaBuilder<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: NumberDataSchemaBuilder<T>) -> Self {
        let NumberDataSchemaBuilder {
            inner,
            maximum,
            minimum,
            multiple_of,
        } = builder;
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
                    default,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                    other,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = inner.into();

        let subtype = Some(UncheckedDataSchemaSubtype::Number(NumberSchema {
            minimum,
            maximum,
            multiple_of,
        }));

        UncheckedDataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<NumberDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: NumberDataSchemaBuilder<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<NumberDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: NumberDataSchemaBuilder<T>) -> Self {
        let NumberDataSchemaBuilder {
            inner,
            maximum,
            minimum,
            multiple_of,
        } = builder;
        let PartialDataSchemaBuilder {
            constant: _,
            default,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = inner.into();

        let subtype = Some(UncheckedDataSchemaSubtype::Number(NumberSchema {
            minimum,
            maximum,
            multiple_of,
        }));

        PartialDataSchema {
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> From<IntegerDataSchemaBuilder<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: IntegerDataSchemaBuilder<T>) -> Self {
        let IntegerDataSchemaBuilder {
            inner,
            maximum,
            minimum,
        } = builder;
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
                    default,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                    other,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = inner.into();

        let subtype = Some(UncheckedDataSchemaSubtype::Integer(IntegerSchema {
            minimum,
            maximum,
        }));

        UncheckedDataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<IntegerDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: IntegerDataSchemaBuilder<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<IntegerDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: IntegerDataSchemaBuilder<T>) -> Self {
        let IntegerDataSchemaBuilder {
            inner,
            maximum,
            minimum,
        } = builder;
        let PartialDataSchemaBuilder {
            constant: _,
            default,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = inner.into();

        let subtype = Some(UncheckedDataSchemaSubtype::Integer(IntegerSchema {
            minimum,
            maximum,
        }));

        PartialDataSchema {
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> From<ObjectDataSchemaBuilder<T, DS, AS, OS>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: ObjectDataSchemaBuilder<T, DS, AS, OS>) -> Self {
        let ObjectDataSchemaBuilder {
            inner,
            properties,
            required,
            other: other_object_schema,
        } = builder;
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
                    default,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                    other: other_data_schema,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = inner.into();

        let properties = properties
            .is_empty()
            .not()
            .then(|| properties.into_iter().collect());
        let required = required.is_empty().not().then_some(required);
        let subtype = Some(UncheckedDataSchemaSubtype::Object(UncheckedObjectSchema {
            properties,
            required,
            other: other_object_schema,
        }));

        UncheckedDataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other: other_data_schema,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<ObjectDataSchemaBuilder<T, DS, AS, OS>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: ObjectDataSchemaBuilder<T, DS, AS, OS>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<ObjectDataSchemaBuilder<T, DS, AS, OS>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: ObjectDataSchemaBuilder<T, DS, AS, OS>) -> Self {
        let ObjectDataSchemaBuilder {
            inner,
            properties,
            required,
            other: other_object_schema,
        } = builder;
        let PartialDataSchemaBuilder {
            constant: _,
            default,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other: other_data_schema,
            _marker: _,
        } = inner.into();

        let properties = properties
            .is_empty()
            .not()
            .then(|| properties.into_iter().collect());
        let required = required.is_empty().not().then_some(required);
        let subtype = Some(UncheckedDataSchemaSubtype::Object(UncheckedObjectSchema {
            properties,
            required,
            other: other_object_schema,
        }));

        PartialDataSchema {
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other: other_data_schema,
        }
    }
}

impl<T, DS, AS, OS> From<StringDataSchemaBuilder<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StringDataSchemaBuilder<T>) -> Self {
        let StringDataSchemaBuilder {
            inner,
            min_length,
            max_length,
            pattern,
            content_encoding,
            content_media_type,
        } = builder;

        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
                    default,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                    other,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = inner.into();

        let subtype = Some(UncheckedDataSchemaSubtype::String(StringSchema {
            min_length,
            max_length,
            pattern,
            content_encoding,
            content_media_type,
        }));

        UncheckedDataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<StringDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: StringDataSchemaBuilder<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<StringDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StringDataSchemaBuilder<T>) -> Self {
        let StringDataSchemaBuilder {
            inner,
            min_length,
            max_length,
            pattern,
            content_encoding,
            content_media_type,
        } = builder;

        let PartialDataSchemaBuilder {
            constant: _,
            default,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = inner.into();

        let subtype = Some(UncheckedDataSchemaSubtype::String(StringSchema {
            min_length,
            max_length,
            pattern,
            content_encoding,
            content_media_type,
        }));

        PartialDataSchema {
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype,
            other,
        }
    }
}

impl<T, DS, AS, OS> From<ReadOnly<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<UncheckedDataSchema<DS, AS, OS>>,
{
    fn from(builder: ReadOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: true,
            ..data_schema
        }
    }
}

impl<T, DS, AS, OS> TryFrom<ReadOnly<T>> for DataSchema<DS, AS, OS>
where
    T: Into<UncheckedDataSchema<DS, AS, OS>>,
{
    type Error = Error;

    fn try_from(value: ReadOnly<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<WriteOnly<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<UncheckedDataSchema<DS, AS, OS>>,
{
    fn from(builder: WriteOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: false,
            ..data_schema
        }
    }
}

impl<T, DS, AS, OS> TryFrom<WriteOnly<T>> for DataSchema<DS, AS, OS>
where
    T: Into<UncheckedDataSchema<DS, AS, OS>>,
{
    type Error = Error;

    fn try_from(value: WriteOnly<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<ReadOnly<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchema<DS, AS, OS>>,
{
    fn from(builder: ReadOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: true,
            ..data_schema
        }
    }
}

impl<T, DS, AS, OS> From<WriteOnly<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchema<DS, AS, OS>>,
{
    fn from(builder: WriteOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: false,
            ..data_schema
        }
    }
}

impl<T, DS, AS, OS> From<EnumDataSchemaBuilder<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: EnumDataSchemaBuilder<T>) -> Self {
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
                    default,
                    unit,
                    one_of: _,
                    enumeration,
                    read_only,
                    write_only,
                    format,
                    other,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = builder.inner.into();

        let enumeration = Some(enumeration);
        Self {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration,
            read_only,
            write_only,
            format,
            subtype: None,
            other,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<EnumDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: EnumDataSchemaBuilder<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<EnumDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: EnumDataSchemaBuilder<T>) -> Self {
        let PartialDataSchemaBuilder {
            constant: _,
            default,
            unit,
            one_of: _,
            enumeration,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = builder.inner.into();

        let enumeration = Some(enumeration);
        Self {
            constant: None,
            default,
            unit,
            one_of: None,
            enumeration,
            read_only,
            write_only,
            format,
            subtype: None,
            other,
        }
    }
}

impl<T, DS, AS, OS> From<OneOfDataSchemaBuilder<T>> for UncheckedDataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: OneOfDataSchemaBuilder<T>) -> Self {
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
                    default,
                    unit,
                    one_of,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                    other,
                    _marker: _,
                },
            info:
                HumanReadableInfo {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                },
        } = builder.inner.into();

        let one_of = Some(one_of);
        Self {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
            default,
            unit,
            one_of,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype: None,
            other,
        }
    }
}

impl<T, DS, AS, OS> TryFrom<OneOfDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    type Error = Error;

    fn try_from(value: OneOfDataSchemaBuilder<T>) -> Result<Self, Self::Error> {
        let data_schema: UncheckedDataSchema<_, _, _> = value.into();
        data_schema.try_into()
    }
}

impl<T, DS, AS, OS> From<OneOfDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: OneOfDataSchemaBuilder<T>) -> Self {
        let PartialDataSchemaBuilder {
            constant: _,
            default,
            unit,
            one_of,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = builder.inner.into();

        let one_of = Some(one_of);
        Self {
            constant: None,
            default,
            unit,
            one_of,
            enumeration: None,
            read_only,
            write_only,
            format,
            subtype: None,
            other,
        }
    }
}

pub(super) trait CheckableDataSchema {
    fn check(&self) -> Result<(), Error>;
}

impl<DS, AS, OS> CheckableDataSchema for UncheckedDataSchema<DS, AS, OS> {
    fn check(&self) -> Result<(), Error> {
        check_data_schema_subtype(&self.subtype)?;
        check_one_of_schema(self.one_of.as_deref())?;
        Ok(())
    }
}

impl<DS, AS, OS> CheckableDataSchema for PartialDataSchema<DS, AS, OS> {
    fn check(&self) -> Result<(), Error> {
        check_data_schema_subtype(&self.subtype)?;
        check_one_of_schema(self.one_of.as_deref())?;
        Ok(())
    }
}

pub(super) fn check_data_schema_subtype<DS, AS, OS>(
    mut subtype: &Option<UncheckedDataSchemaSubtype<DS, AS, OS>>,
) -> Result<(), Error> {
    use UncheckedDataSchemaSubtype::*;

    let mut stack = Vec::new();

    loop {
        if let Some(subtype) = subtype.as_ref() {
            match subtype {
                Array(array) => {
                    match (array.min_items, array.max_items) {
                        (Some(min), Some(max))
                            if matches!(min.partial_cmp(&max), None | Some(Ordering::Greater)) =>
                        {
                            return Err(Error::InvalidMinMax)
                        }
                        _ => {}
                    };

                    if let Some(items) = array.items.as_deref() {
                        stack.extend(items.iter());
                    }
                }
                Number(number) => {
                    match (number.minimum, number.maximum) {
                        (Some(x), _) if x.is_nan() => return Err(Error::NanMinMax),
                        (_, Some(x)) if x.is_nan() => return Err(Error::NanMinMax),
                        (Some(min), Some(max))
                            if matches!(min.partial_cmp(&max), None | Some(Ordering::Greater)) =>
                        {
                            return Err(Error::InvalidMinMax)
                        }
                        _ => {}
                    }

                    match number.multiple_of {
                        Some(multiple_of) if multiple_of <= 0. => {
                            return Err(Error::InvalidMultipleOf)
                        }
                        _ => {}
                    }
                }
                Integer(integer) => match (integer.minimum, integer.maximum) {
                    (Some(min), Some(max))
                        if matches!(min.partial_cmp(&max), None | Some(Ordering::Greater)) =>
                    {
                        return Err(Error::InvalidMinMax)
                    }
                    _ => {}
                },
                Object(UncheckedObjectSchema {
                    properties: Some(properties),
                    ..
                }) => stack.extend(properties.values()),
                Object(_) | String(_) | Boolean | Null => {}
            }
        }

        match stack.pop() {
            Some(new_data_schema) => {
                if let Some(children) = new_data_schema.one_of.as_deref() {
                    stack.extend(children.iter());
                }

                subtype = &new_data_schema.subtype
            }
            None => break Ok(()),
        }
    }
}

fn check_one_of_schema<T>(one_of: Option<&[T]>) -> Result<(), Error>
where
    T: CheckableDataSchema,
{
    one_of
        .map(|one_of| one_of.iter().try_for_each(|schema| schema.check()))
        .unwrap_or(Ok(()))
}

impl<DS, AS, OS> TryFrom<UncheckedDataSchema<DS, AS, OS>> for DataSchema<DS, AS, OS> {
    type Error = Error;

    fn try_from(data_schema: UncheckedDataSchema<DS, AS, OS>) -> Result<Self, Self::Error> {
        let UncheckedDataSchema {
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
            other,
        } = data_schema;

        let titles = titles.map(|titles| titles.build()).transpose()?;
        let descriptions = descriptions
            .map(|descriptions| descriptions.build())
            .transpose()?;
        let one_of = one_of
            .map(|one_of| {
                one_of
                    .into_iter()
                    .map(|data_schema| data_schema.try_into())
                    .collect()
            })
            .transpose()?;
        let subtype = subtype.map(|subtype| subtype.try_into()).transpose()?;

        Ok(Self {
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
            other,
        })
    }
}

pub(crate) fn uri_variables_contains_arrays_objects<Other>(
    uri_variables: &UncheckedDataSchemaMap<Other>,
) -> bool
where
    Other: ExtendableThing,
{
    uri_variables.values().any(|schema| {
        matches!(
            &schema.subtype,
            Some(UncheckedDataSchemaSubtype::Object(_) | UncheckedDataSchemaSubtype::Array(_))
        )
    })
}

impl<DS, AS, OS> TryFrom<UncheckedDataSchemaSubtype<DS, AS, OS>> for DataSchemaSubtype<DS, AS, OS> {
    type Error = Error;

    fn try_from(value: UncheckedDataSchemaSubtype<DS, AS, OS>) -> Result<Self, Self::Error> {
        use UncheckedDataSchemaSubtype::*;

        let out = match value {
            Array(array) => DataSchemaSubtype::Array(array.try_into()?),
            Boolean => DataSchemaSubtype::Boolean,
            Number(number) => DataSchemaSubtype::Number(number),
            Integer(integer) => DataSchemaSubtype::Integer(integer),
            Object(object) => DataSchemaSubtype::Object(object.try_into()?),
            String(string) => DataSchemaSubtype::String(string),
            Null => DataSchemaSubtype::Null,
        };

        Ok(out)
    }
}

impl<DS, AS, OS> TryFrom<UncheckedArraySchema<DS, AS, OS>> for ArraySchema<DS, AS, OS> {
    type Error = Error;

    fn try_from(value: UncheckedArraySchema<DS, AS, OS>) -> Result<Self, Self::Error> {
        let UncheckedArraySchema {
            items,
            min_items,
            max_items,
            other,
        } = value;
        let items = items
            .map(|items| items.into_iter().map(TryInto::try_into).collect())
            .transpose()?;

        Ok(Self {
            items,
            min_items,
            max_items,
            other,
        })
    }
}

impl<DS, AS, OS> TryFrom<UncheckedObjectSchema<DS, AS, OS>> for ObjectSchema<DS, AS, OS> {
    type Error = Error;

    fn try_from(value: UncheckedObjectSchema<DS, AS, OS>) -> Result<Self, Self::Error> {
        let UncheckedObjectSchema {
            properties,
            required,
            other,
        } = value;
        let properties = properties
            .map(|properties| {
                properties
                    .into_iter()
                    .map(|(k, v)| v.try_into().map(|v| (k, v)))
                    .collect()
            })
            .transpose()?;

        Ok(Self {
            properties,
            required,
            other,
        })
    }
}

#[cfg(test)]
mod tests {
    use serde::{Deserialize, Serialize};
    use serde_json::json;

    use crate::{
        extend::ExtendableThing,
        hlist::{Cons, Nil},
        thing::{ArraySchema, DataSchemaFromOther, ObjectSchema},
    };

    use super::*;

    #[test]
    fn null_simple() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().null().try_into().unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Null),
                other: Nil,
            }
        );
    }

    #[test]
    fn null_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> =
            PartialDataSchemaBuilder::default().null().into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Null),
                other: Nil,
            }
        );
    }

    #[test]
    fn boolean_simple() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().bool().try_into().unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Boolean),
                other: Nil,
            }
        );
    }

    #[test]
    fn boolean_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> =
            PartialDataSchemaBuilder::default().bool().into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Boolean),
                other: Nil,
            }
        );
    }

    #[test]
    fn string_simple() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().string().try_into().unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::String(StringSchema {
                    max_length: None,
                    min_length: None,
                    pattern: None,
                    content_encoding: None,
                    content_media_type: None,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn string_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> =
            PartialDataSchemaBuilder::default().string().into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::String(StringSchema {
                    min_length: None,
                    max_length: None,
                    pattern: None,
                    content_encoding: None,
                    content_media_type: None,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn empty_simple_array() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().array().try_into().unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                    items: None,
                    min_items: None,
                    max_items: None,
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn empty_partial_array() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> =
            PartialDataSchemaBuilder::default().array().into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Array(UncheckedArraySchema {
                    items: None,
                    min_items: None,
                    max_items: None,
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn number_simple() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().number().try_into().unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
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
                })),
                other: Nil,
            }
        );
    }
    #[test]
    fn number_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> =
            PartialDataSchemaBuilder::default().number().into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Number(NumberSchema {
                    maximum: None,
                    minimum: None,
                    multiple_of: None,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn integer_simple() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().integer().try_into().unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                    maximum: None,
                    minimum: None
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn partial_simple() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> =
            PartialDataSchemaBuilder::default().integer().into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Integer(IntegerSchema {
                    maximum: None,
                    minimum: None
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn empty_simple_object() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().object().try_into().unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                    properties: None,
                    required: None,
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn empty_partial_object() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> =
            PartialDataSchemaBuilder::default().object().into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Object(UncheckedObjectSchema {
                    properties: None,
                    required: None,
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn constant_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .constant(json!({ "hello": 42 }))
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: Some(json!({
                    "hello": 42,
                })),
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: true,
                write_only: false,
                format: None,
                subtype: None,
                other: Nil,
            }
        );
    }

    #[test]
    fn constant_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> = PartialDataSchemaBuilder::default()
            .constant(json!({ "hello": 42 }))
            .into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: Some(json!({
                    "hello": 42,
                })),
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: true,
                write_only: false,
                format: None,
                subtype: None,
                other: Nil,
            }
        );
    }

    #[test]
    fn enum_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .enumeration("hello")
            .enumeration("world")
            .enumeration(42)
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: Some(vec!["hello".into(), "world".into(), 42.into()]),
                read_only: false,
                write_only: false,
                format: None,
                subtype: None,
                other: Nil,
            }
        );
    }

    #[test]
    fn enum_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> = PartialDataSchemaBuilder::default()
            .enumeration("hello")
            .enumeration("world")
            .enumeration(42)
            .into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: Some(vec!["hello".into(), "world".into(), 42.into()]),
                read_only: false,
                write_only: false,
                format: None,
                subtype: None,
                other: Nil,
            }
        );
    }

    #[test]
    fn read_only_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .bool()
            .read_only()
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: true,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Boolean),
                other: Nil,
            }
        );
    }

    #[test]
    fn read_only_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> = PartialDataSchemaBuilder::default()
            .bool()
            .read_only()
            .into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: true,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Boolean),
                other: Nil,
            }
        );
    }

    #[test]
    fn read_only_to_builder() {
        let data_schema_builder: DataSchemaBuilder<Nil, Nil, Nil, Extended> = ReadOnly {
            inner: DataSchemaBuilder::default(),
        }
        .into();
        assert_eq!(
            data_schema_builder,
            DataSchemaBuilder {
                partial: PartialDataSchemaBuilder {
                    constant: None,
                    default: None,
                    unit: None,
                    one_of: vec![],
                    enumeration: vec![],
                    read_only: true,
                    write_only: false,
                    format: None,
                    other: Nil,
                    _marker: PhantomData,
                },
                info: Default::default(),
            }
        );
    }

    #[test]
    fn read_only_to_partial_builder() {
        let data_schema_builder: PartialDataSchemaBuilder<Nil, Nil, Nil, Extended> = ReadOnly {
            inner: PartialDataSchemaBuilder::default(),
        }
        .into();
        assert_eq!(
            data_schema_builder,
            PartialDataSchemaBuilder {
                constant: None,
                default: None,
                unit: None,
                one_of: vec![],
                enumeration: vec![],
                read_only: true,
                write_only: false,
                format: None,
                other: Nil,
                _marker: PhantomData,
            },
        );
    }

    #[test]
    fn write_only_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .bool()
            .write_only()
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: true,
                format: None,
                subtype: Some(DataSchemaSubtype::Boolean),
                other: Nil,
            }
        );
    }

    #[test]
    fn write_only_partial() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> = PartialDataSchemaBuilder::default()
            .bool()
            .write_only()
            .into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: true,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Boolean),
                other: Nil,
            }
        );
    }

    #[test]
    fn write_only_to_builder() {
        let data_schema_builder: DataSchemaBuilder<Nil, Nil, Nil, Extended> = WriteOnly {
            inner: DataSchemaBuilder::default(),
        }
        .into();
        assert_eq!(
            data_schema_builder,
            DataSchemaBuilder {
                partial: PartialDataSchemaBuilder {
                    constant: None,
                    default: None,
                    unit: None,
                    one_of: vec![],
                    enumeration: vec![],
                    read_only: false,
                    write_only: true,
                    format: None,
                    other: Nil,
                    _marker: PhantomData,
                },
                info: Default::default(),
            }
        );
    }

    #[test]
    fn write_only_to_partial_builder() {
        let data_schema_builder: PartialDataSchemaBuilder<Nil, Nil, Nil, Extended> = WriteOnly {
            inner: PartialDataSchemaBuilder::default(),
        }
        .into();
        assert_eq!(
            data_schema_builder,
            PartialDataSchemaBuilder {
                constant: None,
                default: None,
                unit: None,
                one_of: vec![],
                enumeration: vec![],
                read_only: false,
                write_only: true,
                format: None,
                other: Nil,
                _marker: PhantomData,
            },
        );
    }

    #[test]
    fn null_full() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .null()
            .attype("attype1")
            .attype("attype2")
            .title("title")
            .titles(|b| b.add("en", "title_en").add("it", "title_it"))
            .description("description")
            .descriptions(|b| b.add("en", "description_en").add("it", "description_it"))
            .default_value(["hello", "world"].as_slice())
            .unit("cm")
            .format("format")
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
                title: Some("title".to_string()),
                titles: Some(
                    [("en", "title_en"), ("it", "title_it")]
                        .into_iter()
                        .map(|(a, b)| (a.parse().unwrap(), b.to_string()))
                        .collect()
                ),
                description: Some("description".to_string()),
                descriptions: Some(
                    [("en", "description_en"), ("it", "description_it")]
                        .into_iter()
                        .map(|(a, b)| (a.parse().unwrap(), b.to_string()))
                        .collect()
                ),
                constant: None,
                default: Some(json! { ["hello", "world"]}),
                unit: Some("cm".to_string()),
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: Some("format".to_string()),
                subtype: Some(DataSchemaSubtype::Null),
                other: Nil,
            }
        );
    }

    #[test]
    fn enum_full() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .enumeration("variant1")
            .enumeration("variant2")
            .enumeration(3u32)
            .attype("attype")
            .title("title")
            .titles(|b| b.add("en", "title_en").add("it", "title_it"))
            .description("description")
            .descriptions(|b| b.add("en", "description_en").add("it", "description_it"))
            .default_value(["hello", "world"].as_slice())
            .unit("cm")
            .format("format")
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: Some(vec!["attype".to_string()]),
                title: Some("title".to_string()),
                titles: Some(
                    [("en", "title_en"), ("it", "title_it")]
                        .into_iter()
                        .map(|(a, b)| (a.parse().unwrap(), b.to_string()))
                        .collect()
                ),
                description: Some("description".to_string()),
                descriptions: Some(
                    [("en", "description_en"), ("it", "description_it")]
                        .into_iter()
                        .map(|(a, b)| (a.parse().unwrap(), b.to_string()))
                        .collect()
                ),
                constant: None,
                default: Some(json! { ["hello", "world"]}),
                unit: Some("cm".to_string()),
                one_of: None,
                enumeration: Some(vec!["variant1".into(), "variant2".into(), 3.into()]),
                read_only: false,
                write_only: false,
                format: Some("format".to_string()),
                subtype: None,
                other: Nil,
            }
        );
    }

    #[test]
    fn read_only_enum() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .enumeration("hello")
            .enumeration("world")
            .title("title")
            .default_value(["hello", "world"].as_slice())
            .read_only()
            .enumeration(42)
            .description("description")
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: Some("title".to_string()),
                titles: None,
                description: Some("description".to_string()),
                descriptions: None,
                constant: None,
                default: Some(json! { ["hello", "world"]}),
                unit: None,
                one_of: None,
                enumeration: Some(vec!["hello".into(), "world".into(), 42.into()]),
                read_only: true,
                write_only: false,
                format: None,
                subtype: None,
                other: Nil,
            }
        );
    }

    #[test]
    fn array_with_content() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .array()
            .min_items(0)
            .max_items(5)
            .append(|b| b.finish_extend().constant("hello"))
            .append(|b| b.finish_extend().bool())
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                    items: Some(vec![
                        DataSchema {
                            attype: None,
                            title: None,
                            titles: None,
                            description: None,
                            descriptions: None,
                            constant: Some("hello".into()),
                            default: None,
                            unit: None,
                            one_of: None,
                            enumeration: None,
                            read_only: true,
                            write_only: false,
                            format: None,
                            subtype: None,
                            other: Nil,
                        },
                        DataSchema {
                            attype: None,
                            title: None,
                            titles: None,
                            description: None,
                            descriptions: None,
                            constant: None,
                            default: None,
                            unit: None,
                            one_of: None,
                            enumeration: None,
                            read_only: false,
                            write_only: false,
                            format: None,
                            subtype: Some(DataSchemaSubtype::Boolean),
                            other: Nil,
                        },
                    ]),
                    min_items: Some(0),
                    max_items: Some(5),
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn array_partial_with_content() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> = PartialDataSchemaBuilder::default()
            .array()
            .min_items(0)
            .max_items(5)
            .append(|b| b.finish_extend().constant("hello"))
            .append(|b| b.finish_extend().bool())
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Array(UncheckedArraySchema {
                    items: Some(vec![
                        UncheckedDataSchema {
                            attype: None,
                            title: None,
                            titles: None,
                            description: None,
                            descriptions: None,
                            constant: Some("hello".into()),
                            default: None,
                            unit: None,
                            one_of: None,
                            enumeration: None,
                            read_only: true,
                            write_only: false,
                            format: None,
                            subtype: None,
                            other: Nil,
                        },
                        UncheckedDataSchema {
                            attype: None,
                            title: None,
                            titles: None,
                            description: None,
                            descriptions: None,
                            constant: None,
                            default: None,
                            unit: None,
                            one_of: None,
                            enumeration: None,
                            read_only: false,
                            write_only: false,
                            format: None,
                            subtype: Some(UncheckedDataSchemaSubtype::Boolean),
                            other: Nil,
                        },
                    ]),
                    min_items: Some(0),
                    max_items: Some(5),
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn object_with_content() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .object()
            .property("hello", false, |b| b.finish_extend().bool())
            .property("world", true, |b| b.title("title").finish_extend().number())
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                    properties: Some(
                        [
                            (
                                "hello".to_string(),
                                DataSchema {
                                    attype: None,
                                    title: None,
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    constant: None,
                                    default: None,
                                    unit: None,
                                    one_of: None,
                                    enumeration: None,
                                    read_only: false,
                                    write_only: false,
                                    format: None,
                                    subtype: Some(DataSchemaSubtype::Boolean),
                                    other: Nil,
                                }
                            ),
                            (
                                "world".to_string(),
                                DataSchema {
                                    attype: None,
                                    title: Some("title".to_string()),
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    constant: None,
                                    default: None,
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
                                    })),
                                    other: Nil,
                                }
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                    required: Some(vec!["world".to_string()]),
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn object_partial_with_content() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> = PartialDataSchemaBuilder::default()
            .object()
            .property("hello", false, |b| b.finish_extend().bool())
            .property("world", true, |b| b.finish_extend().title("title").number())
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(UncheckedDataSchemaSubtype::Object(UncheckedObjectSchema {
                    properties: Some(
                        [
                            (
                                "hello".to_string(),
                                UncheckedDataSchema {
                                    attype: None,
                                    title: None,
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    constant: None,
                                    default: None,
                                    unit: None,
                                    one_of: None,
                                    enumeration: None,
                                    read_only: false,
                                    write_only: false,
                                    format: None,
                                    subtype: Some(UncheckedDataSchemaSubtype::Boolean),
                                    other: Nil,
                                }
                            ),
                            (
                                "world".to_string(),
                                UncheckedDataSchema {
                                    attype: None,
                                    title: Some("title".to_string()),
                                    titles: None,
                                    description: None,
                                    descriptions: None,
                                    constant: None,
                                    default: None,
                                    unit: None,
                                    one_of: None,
                                    enumeration: None,
                                    read_only: false,
                                    write_only: false,
                                    format: None,
                                    subtype: Some(UncheckedDataSchemaSubtype::Number(
                                        NumberSchema {
                                            maximum: None,
                                            minimum: None,
                                            multiple_of: None,
                                        }
                                    )),
                                    other: Nil,
                                }
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                    required: Some(vec!["world".to_string()]),
                    other: Nil,
                })),
                other: Nil,
            }
        );
    }

    #[test]
    fn integer_with_data() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .integer()
            .exclusive_minimum(10)
            .maximum(5)
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                    maximum: Some(Maximum::Inclusive(5)),
                    minimum: Some(Minimum::Exclusive(10)),
                })),
                other: Nil,
            },
        );

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .integer()
            .minimum(10)
            .exclusive_maximum(5)
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                    maximum: Some(Maximum::Exclusive(5)),
                    minimum: Some(Minimum::Inclusive(10)),
                })),
                other: Nil,
            },
        );
    }

    #[test]
    fn number_with_data() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .number()
            .exclusive_minimum(10.)
            .maximum(5.)
            .multiple_of(2.)
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                    maximum: Some(Maximum::Inclusive(5.)),
                    minimum: Some(Minimum::Exclusive(10.)),
                    multiple_of: Some(2.),
                })),
                other: Nil,
            },
        );

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .number()
            .minimum(10.)
            .exclusive_maximum(5.)
            .multiple_of(2.)
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                    maximum: Some(Maximum::Exclusive(5.)),
                    minimum: Some(Minimum::Inclusive(10.)),
                    multiple_of: Some(2.),
                })),
                other: Nil,
            },
        );
    }

    #[test]
    fn string_with_data() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .string()
            .min_length(5)
            .max_length(32)
            .pattern("pattern")
            .content_encoding("content encoding")
            .content_media_type("content media type")
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::String(StringSchema {
                    min_length: Some(5),
                    max_length: Some(32),
                    pattern: Some("pattern".to_string()),
                    content_encoding: Some("content encoding".to_string()),
                    content_media_type: Some("content media type".to_string()),
                })),
                other: Nil,
            },
        );
    }

    #[test]
    fn one_of_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| b.finish_extend().number())
            .one_of(|b| b.finish_extend().integer())
            .one_of(|b| b.finish_extend().string())
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: Some(vec![
                    DataSchema {
                        attype: None,
                        title: None,
                        titles: None,
                        description: None,
                        descriptions: None,
                        constant: None,
                        default: None,
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
                        })),
                        other: Nil,
                    },
                    DataSchema {
                        attype: None,
                        title: None,
                        titles: None,
                        description: None,
                        descriptions: None,
                        constant: None,
                        default: None,
                        unit: None,
                        one_of: None,
                        enumeration: None,
                        read_only: false,
                        write_only: false,
                        format: None,
                        subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                            maximum: None,
                            minimum: None,
                        })),
                        other: Nil,
                    },
                    DataSchema {
                        attype: None,
                        title: None,
                        titles: None,
                        description: None,
                        descriptions: None,
                        constant: None,
                        default: None,
                        unit: None,
                        one_of: None,
                        enumeration: None,
                        read_only: false,
                        write_only: false,
                        format: None,
                        subtype: Some(DataSchemaSubtype::String(StringSchema {
                            min_length: None,
                            max_length: None,
                            pattern: None,
                            content_encoding: None,
                            content_media_type: None,
                        })),
                        other: Nil,
                    },
                ]),
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: None,
                other: Nil,
            },
        );
    }

    #[test]
    fn one_of_nested() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .object()
            .property("hello", true, |b| {
                b.finish_extend()
                    .one_of(|b| b.finish_extend().string())
                    .one_of(|b| b.finish_extend().integer())
            })
            .try_into()
            .unwrap();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                default: None,
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                    properties: Some(
                        [(
                            "hello".to_string(),
                            DataSchema {
                                attype: None,
                                title: None,
                                titles: None,
                                description: None,
                                descriptions: None,
                                constant: None,
                                default: None,
                                unit: None,
                                one_of: Some(vec![
                                    DataSchema {
                                        attype: None,
                                        title: None,
                                        titles: None,
                                        description: None,
                                        descriptions: None,
                                        constant: None,
                                        default: None,
                                        unit: None,
                                        one_of: None,
                                        enumeration: None,
                                        read_only: false,
                                        write_only: false,
                                        format: None,
                                        subtype: Some(DataSchemaSubtype::String(StringSchema {
                                            min_length: None,
                                            max_length: None,
                                            pattern: None,
                                            content_encoding: None,
                                            content_media_type: None,
                                        })),
                                        other: Nil,
                                    },
                                    DataSchema {
                                        attype: None,
                                        title: None,
                                        titles: None,
                                        description: None,
                                        descriptions: None,
                                        constant: None,
                                        default: None,
                                        unit: None,
                                        one_of: None,
                                        enumeration: None,
                                        read_only: false,
                                        write_only: false,
                                        format: None,
                                        subtype: Some(DataSchemaSubtype::Integer(IntegerSchema {
                                            maximum: None,
                                            minimum: None,
                                        })),
                                        other: Nil,
                                    },
                                ]),
                                enumeration: None,
                                read_only: false,
                                write_only: false,
                                format: None,
                                subtype: None,
                                other: Nil,
                            }
                        ),]
                        .into_iter()
                        .collect()
                    ),
                    required: Some(vec!["hello".to_string()]),
                    other: Nil,
                })),
                other: Nil,
            },
        );
    }

    #[test]
    fn check_valid_data_schema() {
        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| {
                        b.finish_extend()
                            .number()
                            .minimum(0.)
                            .maximum(5.)
                            .multiple_of(2.)
                    })
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| {
                b.finish_extend()
                    .number()
                    .minimum(20.)
                    .maximum(42.)
                    .multiple_of(7.)
            })
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert!(data_schema.check().is_ok());
    }

    #[test]
    fn check_invalid_data_schema() {
        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(5)
                    .max_items(2)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(5.).maximum(0.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(10).maximum(5))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(42.).maximum(20.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(f64::NAN))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::NanMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(f64::NAN).maximum(42.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::NanMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(20).maximum(10)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.finish_extend()
                    .object()
                    .property("a", false, |b| {
                        b.finish_extend().integer().minimum(10).maximum(20)
                    })
                    .property("b", false, |b| {
                        b.finish_extend().integer().minimum(20).maximum(10)
                    })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.finish_extend().number().minimum(0.).maximum(5.))
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .one_of(|b| {
                b.finish_extend()
                    .one_of(|b| b.finish_extend().number().minimum(20.).maximum(10.))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| {
                        b.finish_extend()
                            .one_of(|b| b.finish_extend().number().minimum(5.).maximum(0.))
                    })
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.finish_extend().number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);
    }

    #[test]
    fn check_invalid_data_schema_with_complex_minmax() {
        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .integer()
            .exclusive_minimum(2)
            .maximum(2)
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .integer()
            .minimum(2)
            .exclusive_maximum(2)
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .number()
            .exclusive_minimum(2.)
            .maximum(2.)
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .number()
            .minimum(2.)
            .exclusive_maximum(2.)
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);
    }

    #[test]
    fn check_invalid_data_schema_multiple_of() {
        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .array()
            .append(|b| b.finish_extend().number().multiple_of(0.))
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMultipleOf);

        let data_schema: UncheckedDataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .array()
            .append(|b| b.finish_extend().number().multiple_of(-2.))
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMultipleOf);
    }

    #[test]
    fn check_valid_partial_data_schema() {
        let data_schema: PartialDataSchema<Nil, Nil, Nil> = PartialDataSchemaBuilder::default()
            .one_of(|b| {
                b.finish_extend()
                    .array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| {
                        b.finish_extend()
                            .number()
                            .minimum(0.)
                            .maximum(5.)
                            .multiple_of(2.)
                    })
                    .append(|b| b.finish_extend().integer().minimum(5).maximum(10))
            })
            .one_of(|b| {
                b.finish_extend()
                    .number()
                    .minimum(20.)
                    .maximum(42.)
                    .multiple_of(3.)
            })
            .one_of(|b| {
                b.finish_extend().object().property("a", false, |b| {
                    b.finish_extend().integer().minimum(10).maximum(20)
                })
            })
            .into();

        assert!(data_schema.check().is_ok());
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct A(i32);
    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct B(String);

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ThingExtA {}

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct DataSchemaExtA {
        a: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ArraySchemaExtA {
        b: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ObjectSchemaExtA {
        c: A,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ThingExtB {}

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct DataSchemaExtB {
        d: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ArraySchemaExtB {
        e: B,
    }

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct ObjectSchemaExtB {
        f: B,
    }

    impl ExtendableThing for ThingExtA {
        type InteractionAffordance = ();
        type PropertyAffordance = ();
        type ActionAffordance = ();
        type EventAffordance = ();
        type Form = ();
        type ExpectedResponse = ();
        type DataSchema = DataSchemaExtA;
        type ObjectSchema = ObjectSchemaExtA;
        type ArraySchema = ArraySchemaExtA;
    }

    impl ExtendableThing for ThingExtB {
        type InteractionAffordance = ();
        type PropertyAffordance = ();
        type ActionAffordance = ();
        type EventAffordance = ();
        type Form = ();
        type ExpectedResponse = ();
        type DataSchema = DataSchemaExtB;
        type ObjectSchema = ObjectSchemaExtB;
        type ArraySchema = ArraySchemaExtB;
    }

    #[test]
    fn extend_data_schema() {
        let data_schema: DataSchemaFromOther<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            DataSchemaBuilder::<Cons<ThingExtB, Cons<ThingExtA, Nil>>, _, _, _>::empty()
                .ext(DataSchemaExtA { a: A(1) })
                .ext_with(|| DataSchemaExtB {
                    d: B("hello".to_string()),
                })
                .finish_extend()
                .title("title")
                .null()
                .try_into()
                .unwrap();

        assert_eq!(
            data_schema,
            DataSchema {
                title: Some("title".to_string()),
                other: Nil::cons(DataSchemaExtA { a: A(1) }).cons(DataSchemaExtB {
                    d: B("hello".to_string())
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
                subtype: Some(DataSchemaSubtype::Null),
            }
        );
    }

    #[test]
    fn extend_data_schema_with_array() {
        let data_schema: DataSchemaFromOther<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            DataSchemaBuilder::<
                Cons<ThingExtB, Cons<ThingExtA, Nil>>,
                Cons<ArraySchemaExtB, Cons<ArraySchemaExtA, Nil>>,
                _,
                _,
            >::empty()
            .ext(DataSchemaExtA { a: A(1) })
            .ext_with(|| DataSchemaExtB {
                d: B("hello".to_string()),
            })
            .finish_extend()
            .title("title")
            .array_ext(|b| {
                b.ext(ArraySchemaExtA { b: A(2) })
                    .ext_with(|| ArraySchemaExtB {
                        e: B("world".to_string()),
                    })
            })
            .max_items(10)
            .try_into()
            .unwrap();

        assert_eq!(
            data_schema,
            DataSchema {
                title: Some("title".to_string()),
                other: Nil::cons(DataSchemaExtA { a: A(1) }).cons(DataSchemaExtB {
                    d: B("hello".to_string())
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
                subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                    other: Nil::cons(ArraySchemaExtA { b: A(2) }).cons(ArraySchemaExtB {
                        e: B("world".to_string())
                    }),
                    max_items: Some(10),
                    items: Default::default(),
                    min_items: Default::default(),
                })),
            }
        );
    }

    #[test]
    fn extend_data_schema_with_object() {
        let data_schema: DataSchemaFromOther<Cons<ThingExtB, Cons<ThingExtA, Nil>>> =
            DataSchemaBuilder::<
                Cons<ThingExtB, Cons<ThingExtA, Nil>>,
                _,
                Cons<ObjectSchemaExtB, Cons<ObjectSchemaExtA, Nil>>,
                _,
            >::empty()
            .ext(DataSchemaExtA { a: A(1) })
            .ext_with(|| DataSchemaExtB {
                d: B("hello".to_string()),
            })
            .finish_extend()
            .title("title")
            .object_ext(|b| {
                b.ext(ObjectSchemaExtA { c: A(2) })
                    .ext_with(|| ObjectSchemaExtB {
                        f: B("world".to_string()),
                    })
            })
            .property("x", false, |b| {
                b.ext(DataSchemaExtA { a: A(3) })
                    .ext(DataSchemaExtB {
                        d: B("other".to_string()),
                    })
                    .finish_extend()
                    .null()
            })
            .try_into()
            .unwrap();

        assert_eq!(
            data_schema,
            DataSchema {
                title: Some("title".to_string()),
                other: Nil::cons(DataSchemaExtA { a: A(1) }).cons(DataSchemaExtB {
                    d: B("hello".to_string())
                }),
                subtype: Some(DataSchemaSubtype::Object(ObjectSchema {
                    other: Nil::cons(ObjectSchemaExtA { c: A(2) }).cons(ObjectSchemaExtB {
                        f: B("world".to_string())
                    }),
                    properties: Some(
                        [(
                            "x".to_string(),
                            DataSchema {
                                other: Nil::cons(DataSchemaExtA { a: A(3) }).cons(DataSchemaExtB {
                                    d: B("other".to_string())
                                }),
                                subtype: Some(DataSchemaSubtype::Null),
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
                    required: None,
                })),
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
            }
        );
    }

    #[test]
    fn valid_unchecked_array_data_schema() {
        let data_schema = UncheckedArraySchema::<Nil, Nil, Nil> {
            items: Some(vec![
                UncheckedDataSchema {
                    titles: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang.add("it", "title1").add("en", "title2");
                        multilang
                    }),
                    descriptions: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang
                            .add("it", "description1")
                            .add("en", "description2");
                        multilang
                    }),
                    ..Default::default()
                },
                UncheckedDataSchema {
                    titles: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang.add("it", "title3").add("en", "title4");
                        multilang
                    }),
                    descriptions: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang
                            .add("it", "description3")
                            .add("en", "description4");
                        multilang
                    }),
                    ..Default::default()
                },
            ]),
            min_items: Some(1),
            ..Default::default()
        };

        assert_eq!(
            ArraySchema::try_from(data_schema).unwrap(),
            ArraySchema {
                items: Some(vec![
                    DataSchema {
                        titles: Some(
                            [
                                ("it".parse().unwrap(), "title1".to_string()),
                                ("en".parse().unwrap(), "title2".to_string())
                            ]
                            .into_iter()
                            .collect()
                        ),
                        descriptions: Some(
                            [
                                ("it".parse().unwrap(), "description1".to_string()),
                                ("en".parse().unwrap(), "description2".to_string())
                            ]
                            .into_iter()
                            .collect()
                        ),
                        ..Default::default()
                    },
                    DataSchema {
                        titles: Some(
                            [
                                ("it".parse().unwrap(), "title3".to_string()),
                                ("en".parse().unwrap(), "title4".to_string())
                            ]
                            .into_iter()
                            .collect()
                        ),
                        descriptions: Some(
                            [
                                ("it".parse().unwrap(), "description3".to_string()),
                                ("en".parse().unwrap(), "description4".to_string())
                            ]
                            .into_iter()
                            .collect()
                        ),
                        ..Default::default()
                    },
                ]),
                min_items: Some(1),
                ..Default::default()
            }
        );
    }

    #[test]
    fn invalid_unchecked_array_data_schema() {
        let data_schema = UncheckedArraySchema::<Nil, Nil, Nil> {
            items: Some(vec![
                UncheckedDataSchema {
                    titles: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang.add("it", "title1").add("en", "title2");
                        multilang
                    }),
                    descriptions: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang
                            .add("it", "description1")
                            .add("en", "description2");
                        multilang
                    }),
                    ..Default::default()
                },
                UncheckedDataSchema {
                    titles: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang.add("it", "title3").add("en", "title4");
                        multilang
                    }),
                    descriptions: Some({
                        let mut multilang = MultiLanguageBuilder::default();
                        multilang
                            .add("it", "description3")
                            .add("e1n", "description4");
                        multilang
                    }),
                    ..Default::default()
                },
            ]),
            min_items: Some(1),
            ..Default::default()
        };

        assert_eq!(
            ArraySchema::try_from(data_schema).unwrap_err(),
            Error::InvalidLanguageTag("e1n".to_string()),
        );
    }

    #[test]
    fn valid_unchecked_object_data_schema() {
        let data_schema = UncheckedObjectSchema::<Nil, Nil, Nil> {
            properties: Some(
                [
                    (
                        "data1".to_string(),
                        UncheckedDataSchema {
                            titles: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang.add("it", "title1").add("en", "title2");
                                multilang
                            }),
                            descriptions: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang
                                    .add("it", "description1")
                                    .add("en", "description2");
                                multilang
                            }),
                            ..Default::default()
                        },
                    ),
                    (
                        "data2".to_string(),
                        UncheckedDataSchema {
                            titles: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang.add("it", "title3").add("en", "title4");
                                multilang
                            }),
                            descriptions: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang
                                    .add("it", "description3")
                                    .add("en", "description4");
                                multilang
                            }),
                            ..Default::default()
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            ),
            ..Default::default()
        };

        assert_eq!(
            ObjectSchema::try_from(data_schema).unwrap(),
            ObjectSchema {
                properties: Some(
                    [
                        (
                            "data1".to_string(),
                            DataSchema {
                                titles: Some(
                                    [
                                        ("it".parse().unwrap(), "title1".to_string()),
                                        ("en".parse().unwrap(), "title2".to_string())
                                    ]
                                    .into_iter()
                                    .collect()
                                ),
                                descriptions: Some(
                                    [
                                        ("it".parse().unwrap(), "description1".to_string()),
                                        ("en".parse().unwrap(), "description2".to_string())
                                    ]
                                    .into_iter()
                                    .collect()
                                ),
                                ..Default::default()
                            }
                        ),
                        (
                            "data2".to_string(),
                            DataSchema {
                                titles: Some(
                                    [
                                        ("it".parse().unwrap(), "title3".to_string()),
                                        ("en".parse().unwrap(), "title4".to_string())
                                    ]
                                    .into_iter()
                                    .collect()
                                ),
                                descriptions: Some(
                                    [
                                        ("it".parse().unwrap(), "description3".to_string()),
                                        ("en".parse().unwrap(), "description4".to_string())
                                    ]
                                    .into_iter()
                                    .collect()
                                ),
                                ..Default::default()
                            }
                        ),
                    ]
                    .into_iter()
                    .collect()
                ),
                ..Default::default()
            }
        );
    }

    #[test]
    fn invalid_unchecked_object_data_schema() {
        let data_schema = UncheckedObjectSchema::<Nil, Nil, Nil> {
            properties: Some(
                [
                    (
                        "data1".to_string(),
                        UncheckedDataSchema {
                            titles: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang.add("it", "title1").add("en", "title2");
                                multilang
                            }),
                            descriptions: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang
                                    .add("it", "description1")
                                    .add("en", "description2");
                                multilang
                            }),
                            ..Default::default()
                        },
                    ),
                    (
                        "data2".to_string(),
                        UncheckedDataSchema {
                            titles: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang.add("it", "title3").add("en", "title4");
                                multilang
                            }),
                            descriptions: Some({
                                let mut multilang = MultiLanguageBuilder::default();
                                multilang
                                    .add("i1t", "description3")
                                    .add("en", "description4");
                                multilang
                            }),
                            ..Default::default()
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            ),
            ..Default::default()
        };

        assert_eq!(
            ObjectSchema::try_from(data_schema).unwrap_err(),
            Error::InvalidLanguageTag("i1t".to_string()),
        )
    }

    #[test]
    fn valid_unchecked_data_schema() {
        let data_schema = UncheckedDataSchema::<Nil, Nil, Nil> {
            attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
            title: Some("title".to_string()),
            titles: Some({
                let mut multilang = MultiLanguageBuilder::default();
                multilang.add("it", "title1").add("en", "title2");
                multilang
            }),
            description: Some("description".to_string()),
            descriptions: Some({
                let mut multilang = MultiLanguageBuilder::default();
                multilang
                    .add("it", "description1")
                    .add("en", "description2");
                multilang
            }),
            unit: Some("unit".to_string()),
            read_only: true,
            write_only: true,
            format: Some("format".to_string()),
            subtype: Some(UncheckedDataSchemaSubtype::Number(NumberSchema {
                maximum: Some(Maximum::Inclusive(5.)),
                ..Default::default()
            })),
            ..Default::default()
        };

        assert_eq!(
            DataSchema::try_from(data_schema).unwrap(),
            DataSchema {
                attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
                title: Some("title".to_string()),
                titles: Some(
                    [
                        ("it".parse().unwrap(), "title1".to_string()),
                        ("en".parse().unwrap(), "title2".to_string())
                    ]
                    .into_iter()
                    .collect()
                ),
                description: Some("description".to_string()),
                descriptions: Some(
                    [
                        ("it".parse().unwrap(), "description1".to_string()),
                        ("en".parse().unwrap(), "description2".to_string())
                    ]
                    .into_iter()
                    .collect()
                ),
                unit: Some("unit".to_string()),
                read_only: true,
                write_only: true,
                format: Some("format".to_string()),
                subtype: Some(DataSchemaSubtype::Number(NumberSchema {
                    maximum: Some(Maximum::Inclusive(5.)),
                    ..Default::default()
                })),
                ..Default::default()
            }
        );
    }

    #[test]
    fn invalid_unchecked_data_schema() {
        let data_schema = UncheckedDataSchema::<Nil, Nil, Nil> {
            attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
            title: Some("title".to_string()),
            titles: Some({
                let mut multilang = MultiLanguageBuilder::default();
                multilang.add("it", "title1").add("en", "title2");
                multilang
            }),
            description: Some("description".to_string()),
            descriptions: Some({
                let mut multilang = MultiLanguageBuilder::default();
                multilang
                    .add("i1t", "description1")
                    .add("en", "description2");
                multilang
            }),
            unit: Some("unit".to_string()),
            read_only: true,
            write_only: true,
            format: Some("format".to_string()),
            subtype: Some(UncheckedDataSchemaSubtype::Number(NumberSchema {
                maximum: Some(Maximum::Inclusive(5.)),
                ..Default::default()
            })),
            ..Default::default()
        };

        assert_eq!(
            DataSchema::try_from(data_schema).unwrap_err(),
            Error::InvalidLanguageTag("i1t".to_string()),
        );
    }
}
