use std::{marker::PhantomData, ops::Not};

use crate::{
    extend::{Extend, Extendable},
    thing::{
        ArraySchema, DataSchema, DataSchemaSubtype, IntegerSchema, NumberSchema, ObjectSchema,
        StringSchema,
    },
};

use super::{
    human_readable_info::{
        impl_delegate_buildable_hr_info, BuildableHumanReadableInfo, HumanReadableInfo,
    },
    Error, Extended, MultiLanguageBuilder, ToExtend,
};

#[derive(Debug, PartialEq)]
pub struct PartialDataSchemaBuilder<DS, AS, OS, Status> {
    constant: Option<Value>,
    unit: Option<String>,
    one_of: Vec<DataSchema<DS, AS, OS>>,
    enumeration: Vec<Value>,
    read_only: bool,
    write_only: bool,
    format: Option<String>,
    other: DS,
    _marker: PhantomData<Status>,
}

impl<DS, AS, OS> PartialDataSchemaBuilder<DS, AS, OS, ToExtend> {
    pub(crate) fn empty() -> PartialDataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>
    where
        DS: Extendable,
    {
        PartialDataSchemaBuilder {
            constant: Default::default(),
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
    pub fn ext_with<F, T>(self, f: F) -> PartialDataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        F: FnOnce() -> T,
        DS: Extend<T>,
    {
        let Self {
            constant,
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

    #[inline]
    pub fn ext<T>(self, t: T) -> PartialDataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        DS: Extend<T>,
    {
        self.ext_with(|| t)
    }

    pub fn finish_extend(self) -> PartialDataSchemaBuilder<DS, AS, OS, Extended> {
        let Self {
            constant,
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

#[derive(Debug, Default, PartialEq)]
pub(super) struct PartialDataSchema<DS, AS, OS> {
    pub(super) constant: Option<Value>,
    pub(super) unit: Option<String>,
    pub(super) one_of: Option<Vec<DataSchema<DS, AS, OS>>>,
    pub(super) enumeration: Option<Vec<Value>>,
    pub(super) read_only: bool,
    pub(super) write_only: bool,
    pub(super) format: Option<String>,
    pub(super) subtype: Option<DataSchemaSubtype<DS, AS, OS>>,
    pub(super) other: DS,
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
    pub fn ext_with<F, T>(self, f: F) -> DataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        F: FnOnce() -> T,
        DS: Extend<T>,
    {
        let Self { partial, info } = self;
        let partial = partial.ext_with(f);
        DataSchemaBuilder { partial, info }
    }

    #[inline]
    pub fn ext<T>(self, t: T) -> DataSchemaBuilder<DS::Target, AS, OS, ToExtend>
    where
        DS: Extend<T>,
    {
        self.ext_with(|| t)
    }

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

pub trait BuildableDataSchema<DS, AS, OS, Status>: Sized {
    fn unit(self, value: impl Into<String>) -> Self;
    fn format(self, value: impl Into<String>) -> Self;
}

pub trait SpecializableDataSchema<DS, AS, OS>: BuildableDataSchema<DS, AS, OS, Extended> {
    type Stateless: BuildableDataSchema<DS, AS, OS, Extended>;
    type ExtendableArray: BuildableDataSchema<DS, AS, OS, Extended>;
    type ExtendedArray: BuildableDataSchema<DS, AS, OS, Extended>;
    type Number: BuildableDataSchema<DS, AS, OS, Extended>;
    type Integer: BuildableDataSchema<DS, AS, OS, Extended>;
    type ExtendableObject: BuildableDataSchema<DS, AS, OS, Extended>;
    type ExtendedObject: BuildableDataSchema<DS, AS, OS, Extended>;
    type String: BuildableDataSchema<DS, AS, OS, Extended>;
    type Constant: BuildableDataSchema<DS, AS, OS, Extended>;

    fn array(self) -> Self::ExtendedArray
    where
        AS: Default;
    fn array_ext<F>(self, f: F) -> Self::ExtendableArray
    where
        F: FnOnce(AS::Empty) -> AS,
        AS: Extendable;
    fn bool(self) -> Self::Stateless;
    fn number(self) -> Self::Number;
    fn integer(self) -> Self::Integer;
    fn object(self) -> Self::ExtendedObject
    where
        OS: Default;
    fn object_ext<F>(self, f: F) -> Self::ExtendableObject
    where
        F: FnOnce(OS::Empty) -> OS,
        OS: Extendable;
    fn string(self) -> Self::String;
    fn null(self) -> Self::Stateless;
    fn constant(self, value: impl Into<Value>) -> Self::Constant;
}

pub trait EnumerableDataSchema<DS, AS, OS, Extended>:
    BuildableDataSchema<DS, AS, OS, Extended>
{
    type Target: BuildableDataSchema<DS, AS, OS, Extended>;

    fn enumeration(self, value: impl Into<Value>) -> Self::Target;
}

pub trait UnionDataSchema<DS, AS, OS>: BuildableDataSchema<DS, AS, OS, Extended> {
    type Target: BuildableDataSchema<DS, AS, OS, Extended>;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<DataSchema<DS, AS, OS>>;
}

pub trait ReadableWriteableDataSchema<DS, AS, OS, Extended>:
    BuildableDataSchema<DS, AS, OS, Extended>
{
    type ReadOnly: BuildableDataSchema<DS, AS, OS, Extended>;
    type WriteOnly: BuildableDataSchema<DS, AS, OS, Extended>;

    fn read_only(self) -> Self::ReadOnly;
    fn write_only(self) -> Self::WriteOnly;
}

pub struct ArrayDataSchemaBuilder<Inner, DS, AS, OS> {
    inner: Inner,
    items: Vec<DataSchema<DS, AS, OS>>,
    min_items: Option<u32>,
    max_items: Option<u32>,
    other: AS,
}

pub struct NumberDataSchemaBuilder<Inner> {
    inner: Inner,
    maximum: Option<f64>,
    minimum: Option<f64>,
    multiple_of: Option<f64>,
}

pub struct IntegerDataSchemaBuilder<Inner> {
    inner: Inner,
    maximum: Option<usize>,
    minimum: Option<usize>,
}

pub struct ObjectDataSchemaBuilder<Inner, DS, AS, OS> {
    inner: Inner,
    properties: Vec<(String, DataSchema<DS, AS, OS>)>,
    required: Vec<String>,
    other: OS,
}

pub struct StringDataSchemaBuilder<Inner> {
    inner: Inner,
    max_length: Option<u32>,
}

pub struct EnumDataSchemaBuilder<Inner> {
    inner: Inner,
}

pub struct OneOfDataSchemaBuilder<Inner> {
    inner: Inner,
}

pub enum StatelessDataSchemaType {
    Boolean,
    Null,
}

pub struct StatelessDataSchemaBuilder<Inner> {
    inner: Inner,
    ty: Option<StatelessDataSchemaType>,
}

pub struct ReadOnly<Inner> {
    inner: Inner,
}

pub struct WriteOnly<Inner> {
    inner: Inner,
}

macro_rules! opt_field_decl {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            fn $field(self, value: $ty) -> Self;
        )*
    };
}

pub trait ArrayDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(min_items: u32, max_items: u32);

    fn append<F, T>(self, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<DataSchema<DS, AS, OS>>;
}

pub trait NumberDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(minimum: f64, maximum: f64, multiple_of: f64);
}

pub trait IntegerDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(minimum: usize, maximum: usize);
}

pub trait ObjectDataSchemaBuilderLike<DS, AS, OS> {
    fn property<F, T>(self, name: impl Into<String>, required: bool, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder<<DS as Extendable>::Empty, AS, OS, ToExtend>) -> T,
        DS: Extendable,
        T: Into<DataSchema<DS, AS, OS>>;
}

pub trait StringDataSchemaBuilderLike<DS, AS, OS> {
    opt_field_decl!(max_length: u32);
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
        T: Into<DataSchema<DS, AS, OS>>,
    {
        self.items
            .push(f(DataSchemaBuilder::<DS, _, _, _>::empty()).into());
        self
    }
}

impl<Inner: BuildableDataSchema<DS, AS, OS, Extended>, DS, AS, OS>
    NumberDataSchemaBuilderLike<DS, AS, OS> for NumberDataSchemaBuilder<Inner>
{
    opt_field_builder!(minimum: f64, maximum: f64, multiple_of: f64);
}

impl<Inner: BuildableDataSchema<DS, AS, OS, Extended>, DS, AS, OS>
    IntegerDataSchemaBuilderLike<DS, AS, OS> for IntegerDataSchemaBuilder<Inner>
{
    opt_field_builder!(minimum: usize, maximum: usize);
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
        T: Into<DataSchema<DS, AS, OS>>,
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
    opt_field_builder!(max_length: u32);
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
            T: Into<crate::thing::DataSchema<DS, AS, OS>>,
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
            T: Into<crate::thing::DataSchema<DS, AS, OS>>,
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
                type ExtendableArray = ArrayDataSchemaBuilder<Self, DS, AS, OS>;
                type ExtendedArray = ArrayDataSchemaBuilder<Self, DS, AS, OS>;
                type Number = NumberDataSchemaBuilder<Self>;
                type Integer = IntegerDataSchemaBuilder<Self>;
                type ExtendableObject = ObjectDataSchemaBuilder<Self, DS, AS, OS>;
                type ExtendedObject = ObjectDataSchemaBuilder<Self, DS, AS, OS>;
                type String = StringDataSchemaBuilder<Self>;
                type Constant = ReadOnly<StatelessDataSchemaBuilder<Self>>;

                fn array(self) -> Self::ExtendedArray
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

                fn array_ext<F>(self, f: F) -> Self::ExtendableArray
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

                fn object(self) -> Self::ExtendedObject
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

                fn object_ext<F>(self, f: F) -> Self::ExtendableObject
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
                        max_length: Default::default(),
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
                    T: Into<DataSchema<DS, AS, OS>>,
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
        T: Into<DataSchema<DS, AS, OS>>,
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
        T: Into<DataSchema<DS, AS, OS>>,
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
        T: Into<DataSchema<DS, AS, OS>>,
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
        T: Into<DataSchema<DS, AS, OS>>,
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

impl<DS, AS, OS> From<StatelessDataSchemaType> for DataSchemaSubtype<DS, AS, OS> {
    fn from(ty: StatelessDataSchemaType) -> Self {
        match ty {
            StatelessDataSchemaType::Boolean => DataSchemaSubtype::Boolean,
            StatelessDataSchemaType::Null => DataSchemaSubtype::Null,
        }
    }
}

impl<T, DS, AS, OS> From<StatelessDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StatelessDataSchemaBuilder<T>) -> Self {
        let StatelessDataSchemaBuilder { inner, ty } = builder;
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant,
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

        DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant,
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

impl<T, DS, AS, OS> From<StatelessDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StatelessDataSchemaBuilder<T>) -> Self {
        let StatelessDataSchemaBuilder { inner, ty } = builder;
        let PartialDataSchemaBuilder {
            constant,
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

impl<T, DS, AS, OS> From<ArrayDataSchemaBuilder<T, DS, AS, OS>> for DataSchema<DS, AS, OS>
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

        let items = items.is_empty().not().then(|| items);
        let subtype = Some(DataSchemaSubtype::Array(ArraySchema {
            items,
            min_items,
            max_items,
            other: other_array_schema,
        }));

        DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
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
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other: other_data_schema,
            _marker: _,
        } = inner.into();

        let items = items.is_empty().not().then(|| items);
        let subtype = Some(DataSchemaSubtype::Array(ArraySchema {
            items,
            min_items,
            max_items,
            other: other_array_schema,
        }));

        PartialDataSchema {
            constant: None,
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

impl<T, DS, AS, OS> From<NumberDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
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

        let subtype = Some(DataSchemaSubtype::Number(NumberSchema {
            minimum,
            maximum,
            multiple_of,
        }));

        DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
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
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = inner.into();

        let subtype = Some(DataSchemaSubtype::Number(NumberSchema {
            minimum,
            maximum,
            multiple_of,
        }));

        PartialDataSchema {
            constant: None,
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

impl<T, DS, AS, OS> From<IntegerDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
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

        let subtype = Some(DataSchemaSubtype::Integer(IntegerSchema {
            minimum,
            maximum,
        }));

        DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
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
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = inner.into();

        let subtype = Some(DataSchemaSubtype::Integer(IntegerSchema {
            minimum,
            maximum,
        }));

        PartialDataSchema {
            constant: None,
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

impl<T, DS, AS, OS> From<ObjectDataSchemaBuilder<T, DS, AS, OS>> for DataSchema<DS, AS, OS>
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
        let required = required.is_empty().not().then(|| required);
        let subtype = Some(DataSchemaSubtype::Object(ObjectSchema {
            properties,
            required,
            other: other_object_schema,
        }));

        DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
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
        let required = required.is_empty().not().then(|| required);
        let subtype = Some(DataSchemaSubtype::Object(ObjectSchema {
            properties,
            required,
            other: other_object_schema,
        }));

        PartialDataSchema {
            constant: None,
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

impl<T, DS, AS, OS> From<StringDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StringDataSchemaBuilder<T>) -> Self {
        let StringDataSchemaBuilder { inner, max_length } = builder;
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
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

        let subtype = Some(DataSchemaSubtype::String(StringSchema { max_length }));

        DataSchema {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: None,
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

impl<T, DS, AS, OS> From<StringDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: StringDataSchemaBuilder<T>) -> Self {
        let StringDataSchemaBuilder { inner, max_length } = builder;
        let PartialDataSchemaBuilder {
            constant: _,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
            other,
            _marker: _,
        } = inner.into();

        let subtype = Some(DataSchemaSubtype::String(StringSchema { max_length }));

        PartialDataSchema {
            constant: None,
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

impl<T, DS, AS, OS> From<ReadOnly<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchema<DS, AS, OS>>,
{
    fn from(builder: ReadOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: true,
            ..data_schema
        }
    }
}

impl<T, DS, AS, OS> From<WriteOnly<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchema<DS, AS, OS>>,
{
    fn from(builder: WriteOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: false,
            ..data_schema
        }
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

impl<T, DS, AS, OS> From<EnumDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: EnumDataSchemaBuilder<T>) -> Self {
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
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

impl<T, DS, AS, OS> From<EnumDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: EnumDataSchemaBuilder<T>) -> Self {
        let PartialDataSchemaBuilder {
            constant: _,
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

impl<T, DS, AS, OS> From<OneOfDataSchemaBuilder<T>> for DataSchema<DS, AS, OS>
where
    T: Into<DataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: OneOfDataSchemaBuilder<T>) -> Self {
        let DataSchemaBuilder {
            partial:
                PartialDataSchemaBuilder {
                    constant: _,
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

impl<T, DS, AS, OS> From<OneOfDataSchemaBuilder<T>> for PartialDataSchema<DS, AS, OS>
where
    T: Into<PartialDataSchemaBuilder<DS, AS, OS, Extended>>,
{
    fn from(builder: OneOfDataSchemaBuilder<T>) -> Self {
        let PartialDataSchemaBuilder {
            constant: _,
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

impl<DS, AS, OS> CheckableDataSchema for DataSchema<DS, AS, OS> {
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
    mut subtype: &Option<DataSchemaSubtype<DS, AS, OS>>,
) -> Result<(), Error> {
    use DataSchemaSubtype::*;

    let mut stack = Vec::new();

    loop {
        if let Some(subtype) = subtype.as_ref() {
            match subtype {
                Array(array) => {
                    match (array.min_items, array.max_items) {
                        (Some(min), Some(max)) if min > max => return Err(Error::InvalidMinMax),
                        _ => {}
                    };

                    if let Some(items) = array.items.as_deref() {
                        stack.extend(items.iter());
                    }
                }
                Number(number) => {
                    match (number.minimum, number.maximum) {
                        (Some(x), _) | (_, Some(x)) if x.is_nan() => return Err(Error::NanMinMax),
                        (Some(min), Some(max)) if min > max => return Err(Error::InvalidMinMax),
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
                    (Some(min), Some(max)) if min > max => return Err(Error::InvalidMinMax),
                    _ => {}
                },
                Object(ObjectSchema {
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
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default().null().into();
        assert_eq!(
            data_schema,
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
    fn boolean_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default().bool().into();
        assert_eq!(
            data_schema,
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
    fn string_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default().string().into();
        assert_eq!(
            data_schema,
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
                subtype: Some(DataSchemaSubtype::String(StringSchema { max_length: None })),
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
                unit: None,
                one_of: None,
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: Some(DataSchemaSubtype::String(StringSchema { max_length: None })),
                other: Nil,
            }
        );
    }

    #[test]
    fn empty_simple_array() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default().array().into();
        assert_eq!(
            data_schema,
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
    fn number_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default().number().into();
        assert_eq!(
            data_schema,
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
    fn integer_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default().integer().into();
        assert_eq!(
            data_schema,
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
    fn empty_simple_object() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default().object().into();
        assert_eq!(
            data_schema,
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
    fn constant_simple() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .constant(json!({ "hello": 42 }))
            .into();
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
            .into();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
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
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().bool().read_only().into();
        assert_eq!(
            data_schema,
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
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().bool().write_only().into();
        assert_eq!(
            data_schema,
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
            .titles(|b| b.cons("en", "title_en").cons("it", "title_it"))
            .description("description")
            .descriptions(|b| b.cons("en", "description_en").cons("it", "description_it"))
            .unit("cm")
            .format("format")
            .into();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: Some(vec!["attype1".to_string(), "attype2".to_string()]),
                title: Some("title".to_string()),
                titles: Some(
                    [("en", "title_en"), ("it", "title_it")]
                        .into_iter()
                        .map(|(a, b)| (a.to_string(), b.to_string()))
                        .collect()
                ),
                description: Some("description".to_string()),
                descriptions: Some(
                    [("en", "description_en"), ("it", "description_it")]
                        .into_iter()
                        .map(|(a, b)| (a.to_string(), b.to_string()))
                        .collect()
                ),
                constant: None,
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
            .titles(|b| b.cons("en", "title_en").cons("it", "title_it"))
            .description("description")
            .descriptions(|b| b.cons("en", "description_en").cons("it", "description_it"))
            .unit("cm")
            .format("format")
            .into();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: Some(vec!["attype".to_string()]),
                title: Some("title".to_string()),
                titles: Some(
                    [("en", "title_en"), ("it", "title_it")]
                        .into_iter()
                        .map(|(a, b)| (a.to_string(), b.to_string()))
                        .collect()
                ),
                description: Some("description".to_string()),
                descriptions: Some(
                    [("en", "description_en"), ("it", "description_it")]
                        .into_iter()
                        .map(|(a, b)| (a.to_string(), b.to_string()))
                        .collect()
                ),
                constant: None,
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
            .read_only()
            .enumeration(42)
            .description("description")
            .into();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: Some("title".to_string()),
                titles: None,
                description: Some("description".to_string()),
                descriptions: None,
                constant: None,
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
            .into();
        assert_eq!(
            data_schema,
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
                subtype: Some(DataSchemaSubtype::Array(ArraySchema {
                    items: Some(vec![
                        DataSchema {
                            attype: None,
                            title: None,
                            titles: None,
                            description: None,
                            descriptions: None,
                            constant: Some("hello".into()),
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
            .into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
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
    fn object_with_content() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .object()
            .property("hello", false, |b| b.finish_extend().bool())
            .property("world", true, |b| b.title("title").finish_extend().number())
            .into();
        assert_eq!(
            data_schema,
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
            .into();
        assert_eq!(
            data_schema,
            PartialDataSchema {
                constant: None,
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
    fn integer_with_data() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .integer()
            .minimum(10)
            .maximum(5)
            .into();
        assert_eq!(
            data_schema,
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
                    maximum: Some(5),
                    minimum: Some(10),
                })),
                other: Nil,
            },
        );
    }

    #[test]
    fn number_with_data() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .number()
            .minimum(10.)
            .maximum(5.)
            .multiple_of(2.)
            .into();
        assert_eq!(
            data_schema,
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
                    maximum: Some(5.),
                    minimum: Some(10.),
                    multiple_of: Some(2.),
                })),
                other: Nil,
            },
        );
    }

    #[test]
    fn string_with_data() {
        let data_schema: DataSchemaFromOther<Nil> =
            DataSchemaBuilder::default().string().max_length(32).into();
        assert_eq!(
            data_schema,
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
                subtype: Some(DataSchemaSubtype::String(StringSchema {
                    max_length: Some(32),
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
            .into();
        assert_eq!(
            data_schema,
            DataSchema {
                attype: None,
                title: None,
                titles: None,
                description: None,
                descriptions: None,
                constant: None,
                unit: None,
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
                        unit: None,
                        one_of: None,
                        enumeration: None,
                        read_only: false,
                        write_only: false,
                        format: None,
                        subtype: Some(DataSchemaSubtype::String(StringSchema { max_length: None })),
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
            .into();
        assert_eq!(
            data_schema,
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
                                unit: None,
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
                                        subtype: Some(DataSchemaSubtype::String(StringSchema {
                                            max_length: None
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
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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
    fn check_invalid_data_schema_multiple_of() {
        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
            .array()
            .append(|b| b.finish_extend().number().multiple_of(0.))
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMultipleOf);

        let data_schema: DataSchemaFromOther<Nil> = DataSchemaBuilder::default()
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
                .into();

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
            .into();

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
            .into();

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
                unit: Default::default(),
                one_of: Default::default(),
                enumeration: Default::default(),
                read_only: Default::default(),
                write_only: Default::default(),
                format: Default::default(),
            }
        );
    }
}
