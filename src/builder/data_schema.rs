use std::ops::Not;

use crate::thing::{
    ArraySchema, DataSchema, DataSchemaSubtype, IntegerSchema, NumberSchema, ObjectSchema,
    StringSchema,
};

use super::{
    human_readable_info::{
        impl_delegate_buildable_hr_info, BuildableHumanReadableInfo, HumanReadableInfo,
    },
    Error, MultiLanguageBuilder,
};

#[derive(Debug, Default, PartialEq)]
pub struct PartialDataSchemaBuilder {
    constant: Option<Value>,
    unit: Option<String>,
    one_of: Vec<DataSchema>,
    enumeration: Vec<Value>,
    read_only: bool,
    write_only: bool,
    format: Option<String>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub(super) struct PartialDataSchema {
    pub(super) constant: Option<Value>,
    pub(super) unit: Option<String>,
    pub(super) one_of: Option<Vec<DataSchema>>,
    pub(super) enumeration: Option<Vec<Value>>,
    pub(super) read_only: bool,
    pub(super) write_only: bool,
    pub(super) format: Option<String>,
    pub(super) subtype: Option<DataSchemaSubtype>,
}

/// Basic builder for [`DataSchema`].
///
/// # Example
///
/// ```compile_fail
/// # use wot_rust::{
/// #   thing::DataSchema,
/// #   builder::DataSchemaBuilder,
/// # };
/// let data_schema: DataSchema = DataSchemaBuilder::default().into();
/// ```
#[derive(Debug, Default, PartialEq)]
pub struct DataSchemaBuilder {
    partial: PartialDataSchemaBuilder,
    info: HumanReadableInfo,
}

trait IntoDataSchema: Into<Self::Target> {
    type Target: Sized;
}

pub trait BuildableDataSchema: Sized {
    fn unit(self, value: impl Into<String>) -> Self;
    fn format(self, value: impl Into<String>) -> Self;
}

pub trait SpecializableDataSchema: BuildableDataSchema {
    type Stateless: BuildableDataSchema;
    type Array: BuildableDataSchema;
    type Number: BuildableDataSchema;
    type Integer: BuildableDataSchema;
    type Object: BuildableDataSchema;
    type String: BuildableDataSchema;
    type Constant: BuildableDataSchema;

    fn array(self) -> Self::Array;
    fn bool(self) -> Self::Stateless;
    fn number(self) -> Self::Number;
    fn integer(self) -> Self::Integer;
    fn object(self) -> Self::Object;
    fn string(self) -> Self::String;
    fn null(self) -> Self::Stateless;
    fn constant(self, value: impl Into<Value>) -> Self::Constant;
}

pub trait EnumerableDataSchema: BuildableDataSchema {
    type Target: BuildableDataSchema;

    fn enumeration(self, value: impl Into<Value>) -> Self::Target;
}

pub trait UnionDataSchema: BuildableDataSchema {
    type Target: BuildableDataSchema;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>;
}

pub trait ReadableWriteableDataSchema: BuildableDataSchema {
    type ReadOnly: BuildableDataSchema;
    type WriteOnly: BuildableDataSchema;

    fn read_only(self) -> Self::ReadOnly;
    fn write_only(self) -> Self::WriteOnly;
}

pub struct ArrayDataSchemaBuilder<Inner> {
    inner: Inner,
    items: Vec<DataSchema>,
    min_items: Option<u32>,
    max_items: Option<u32>,
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

pub struct ObjectDataSchemaBuilder<Inner> {
    inner: Inner,
    properties: Vec<(String, DataSchema)>,
    required: Vec<String>,
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

pub trait ArrayDataSchemaBuilderLike {
    opt_field_decl!(min_items: u32, max_items: u32);

    fn append<F, T>(self, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>;
}

pub trait NumberDataSchemaBuilderLike {
    opt_field_decl!(minimum: f64, maximum: f64, multiple_of: f64);
}

pub trait IntegerDataSchemaBuilderLike {
    opt_field_decl!(minimum: usize, maximum: usize);
}

pub trait ObjectDataSchemaBuilderLike {
    fn property<F, T>(self, name: impl Into<String>, required: bool, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>;
}

pub trait StringDataSchemaBuilderLike {
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

impl<Inner: BuildableDataSchema> ArrayDataSchemaBuilderLike for ArrayDataSchemaBuilder<Inner> {
    opt_field_builder!(min_items: u32, max_items: u32);

    fn append<F, T>(mut self, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        self.items.push(f(DataSchemaBuilder::default()).into());
        self
    }
}

impl<Inner: BuildableDataSchema> NumberDataSchemaBuilderLike for NumberDataSchemaBuilder<Inner> {
    opt_field_builder!(minimum: f64, maximum: f64, multiple_of: f64);
}

impl<Inner: BuildableDataSchema> IntegerDataSchemaBuilderLike for IntegerDataSchemaBuilder<Inner> {
    opt_field_builder!(minimum: usize, maximum: usize);
}

impl<Inner: BuildableDataSchema> ObjectDataSchemaBuilderLike for ObjectDataSchemaBuilder<Inner> {
    fn property<F, T>(mut self, name: impl Into<String>, required: bool, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let data_schema = f(DataSchemaBuilder::default()).into();
        let name = name.into();

        if required {
            self.required.push(name.clone());
        }

        self.properties.push((name, data_schema));
        self
    }
}

impl<Inner: BuildableDataSchema> StringDataSchemaBuilderLike for StringDataSchemaBuilder<Inner> {
    opt_field_builder!(max_length: u32);
}

macro_rules! impl_delegate_schema_builder_like {
    ($( $ty:ident <$( $generic:ident ),+> on $inner:ident ),+ $(,)?) => {
        $(
            impl< $($generic: crate::builder::data_schema::ArrayDataSchemaBuilderLike),+ > crate::builder::data_schema::ArrayDataSchemaBuilderLike for $ty< $($generic),+ > {
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
                    F: FnOnce(crate::builder::data_schema::DataSchemaBuilder) -> T,
                    T: Into<crate::thing::DataSchema>,
                {
                    self.$inner = self.$inner.append(f);
                    self
                }
            }

            impl< $($generic: crate::builder::data_schema::NumberDataSchemaBuilderLike),+ > crate::builder::data_schema::NumberDataSchemaBuilderLike for $ty< $($generic),+ > {
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
            }

            impl< $($generic: crate::builder::data_schema::IntegerDataSchemaBuilderLike),+ > crate::builder::data_schema::IntegerDataSchemaBuilderLike for $ty< $($generic),+ > {
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
            }

            impl< $($generic: crate::builder::data_schema::ObjectDataSchemaBuilderLike),+ > crate::builder::data_schema::ObjectDataSchemaBuilderLike for $ty< $($generic),+ > {
                #[inline]
                fn property<F, T>(mut self, name: impl Into<String>, required: bool, f: F) -> Self
                where
                    F: FnOnce(crate::builder::data_schema::DataSchemaBuilder) -> T,
                    T: Into<crate::thing::DataSchema>,
                {
                    self.$inner = self.$inner.property(name, required, f);
                    self
                }
            }
        )+
    };
}
pub(super) use impl_delegate_schema_builder_like;

impl_delegate_schema_builder_like!(ReadOnly<Inner> on inner, WriteOnly<Innner> on inner);

macro_rules! buildable_data_schema_delegate {
    ($self:ident . $field:ident -> $fn:ident($($arg:ident),*)) => {{
        $self.$field = $self.$field.$fn($($arg),*);
        $self
    }};
}

macro_rules! impl_delegate_buildable_data_schema {
    () => {};

    ($kind:ident $(<$($ty:ident),+>)? : $inner:ident $(, $($rest:tt)*)?) => {
        impl $(<$($ty),+>)? crate::builder::data_schema::BuildableDataSchema for $kind $(<$($ty),+>)?
        $(
            where
                $($ty: crate::builder::data_schema::BuildableDataSchema),+
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
    DataSchemaBuilder: partial,
    ArrayDataSchemaBuilder<Inner>,
    NumberDataSchemaBuilder<Inner>,
    IntegerDataSchemaBuilder<Inner>,
    ObjectDataSchemaBuilder<Inner>,
    StringDataSchemaBuilder<Inner>,
    StatelessDataSchemaBuilder<Inner>,
    ReadOnly<Inner>,
    WriteOnly<Inner>,
    EnumDataSchemaBuilder<Inner>,
    OneOfDataSchemaBuilder<Inner>,
);

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
    DataSchemaBuilder on info,
);

impl BuildableDataSchema for PartialDataSchemaBuilder {
    trait_opt_field_builder!(unit: String, format: String);
}

impl_delegate_buildable_hr_info!(
    ArrayDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    NumberDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    IntegerDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
    ObjectDataSchemaBuilder<Inner: BuildableHumanReadableInfo> on inner,
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
            impl SpecializableDataSchema for $ty {
                type Stateless = StatelessDataSchemaBuilder<Self>;
                type Array = ArrayDataSchemaBuilder<Self>;
                type Number = NumberDataSchemaBuilder<Self>;
                type Integer = IntegerDataSchemaBuilder<Self>;
                type Object = ObjectDataSchemaBuilder<Self>;
                type String = StringDataSchemaBuilder<Self>;
                type Constant = ReadOnly<StatelessDataSchemaBuilder<Self>>;

                fn array(self) -> Self::Array {
                    ArrayDataSchemaBuilder {
                        inner: self,
                        items: Default::default(),
                        min_items: Default::default(),
                        max_items: Default::default(),
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

                fn object(self) -> Self::Object {
                    ObjectDataSchemaBuilder {
                        inner: self,
                        properties: Default::default(),
                        required: Default::default(),
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

impl_specializable_data_schema!(PartialDataSchemaBuilder, DataSchemaBuilder: partial);

macro_rules! impl_enumerable_data_schema {
    ($($ty:ty $( : $($inner_path:ident).+ )? ),+ $(,)?) => {
        $(
        impl EnumerableDataSchema for $ty {
            type Target = EnumDataSchemaBuilder<Self>;

            fn enumeration(mut self, value: impl Into<Value>) -> EnumDataSchemaBuilder<Self> {
                self $(. $($inner_path).+ )?.enumeration.push(value.into());
                EnumDataSchemaBuilder { inner: self }
            }
        }
        )+
    };
}

impl_enumerable_data_schema!(PartialDataSchemaBuilder, DataSchemaBuilder: partial);

impl<Inner> EnumerableDataSchema for ReadOnly<Inner>
where
    Inner: EnumerableDataSchema,
{
    type Target = ReadOnly<Inner::Target>;

    #[inline]
    fn enumeration(self, value: impl Into<Value>) -> Self::Target {
        let Self { inner } = self;

        let inner = inner.enumeration(value);
        ReadOnly { inner }
    }
}

impl<Inner> EnumerableDataSchema for WriteOnly<Inner>
where
    Inner: EnumerableDataSchema,
{
    type Target = WriteOnly<Inner::Target>;

    #[inline]
    fn enumeration(self, value: impl Into<Value>) -> Self::Target {
        let Self { inner } = self;

        let inner = inner.enumeration(value);
        WriteOnly { inner }
    }
}

impl EnumerableDataSchema for EnumDataSchemaBuilder<PartialDataSchemaBuilder> {
    type Target = Self;

    #[inline]
    fn enumeration(mut self, value: impl Into<Value>) -> Self::Target {
        self.inner.enumeration.push(value.into());
        self
    }
}

impl EnumerableDataSchema for EnumDataSchemaBuilder<DataSchemaBuilder> {
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
            impl UnionDataSchema for $ty {
                type Target = OneOfDataSchemaBuilder<Self>;

                fn one_of<F, T>(mut self, f: F) -> Self::Target
                where
                    F: FnOnce(DataSchemaBuilder) -> T,
                    T: Into<DataSchema>,
                {
                    self $(. $($inner_path).+ )? .one_of.push(f(DataSchemaBuilder::default()).into());
                    OneOfDataSchemaBuilder { inner: self }
                }
            }
        )+
    };
}

impl_union_data_schema!(PartialDataSchemaBuilder, DataSchemaBuilder: partial);

impl<Inner> UnionDataSchema for ReadOnly<Inner>
where
    Inner: UnionDataSchema,
{
    type Target = ReadOnly<Inner::Target>;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self { inner } = self;
        let inner = inner.one_of(f);
        ReadOnly { inner }
    }
}

impl<Inner> UnionDataSchema for WriteOnly<Inner>
where
    Inner: UnionDataSchema,
{
    type Target = WriteOnly<Inner::Target>;

    fn one_of<F, T>(self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        let Self { inner } = self;
        let inner = inner.one_of(f);
        WriteOnly { inner }
    }
}

impl UnionDataSchema for OneOfDataSchemaBuilder<PartialDataSchemaBuilder> {
    type Target = Self;

    fn one_of<F, T>(mut self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        self.inner.one_of.push(f(Default::default()).into());
        self
    }
}

impl UnionDataSchema for OneOfDataSchemaBuilder<DataSchemaBuilder> {
    type Target = Self;

    fn one_of<F, T>(mut self, f: F) -> Self::Target
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        self.inner.partial.one_of.push(f(Default::default()).into());
        self
    }
}

macro_rules! impl_rw_data_schema {
    ($( $ty:ty; $($inner_path:ident).+ ),+ $(,)?) => {
        $(
            impl ReadableWriteableDataSchema for $ty
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
    StatelessDataSchemaBuilder<DataSchemaBuilder>; inner.partial,
    StatelessDataSchemaBuilder<PartialDataSchemaBuilder>; inner,
    ArrayDataSchemaBuilder<DataSchemaBuilder>; inner.partial,
    ArrayDataSchemaBuilder<PartialDataSchemaBuilder>; inner,
    NumberDataSchemaBuilder<DataSchemaBuilder>; inner.partial,
    NumberDataSchemaBuilder<PartialDataSchemaBuilder>; inner,
    IntegerDataSchemaBuilder<DataSchemaBuilder>; inner.partial,
    IntegerDataSchemaBuilder<PartialDataSchemaBuilder>; inner,
    ObjectDataSchemaBuilder<DataSchemaBuilder>; inner.partial,
    ObjectDataSchemaBuilder<PartialDataSchemaBuilder>; inner,
    StringDataSchemaBuilder<DataSchemaBuilder>; inner.partial,
    StringDataSchemaBuilder<PartialDataSchemaBuilder>; inner,
    EnumDataSchemaBuilder<DataSchemaBuilder>; inner.partial,
    EnumDataSchemaBuilder<PartialDataSchemaBuilder>; inner,
);

impl<T> From<ReadOnly<T>> for DataSchemaBuilder
where
    T: Into<DataSchemaBuilder>,
{
    fn from(data_schema: ReadOnly<T>) -> Self {
        let DataSchemaBuilder { mut partial, info } = data_schema.inner.into();
        partial.read_only = true;

        Self { partial, info }
    }
}

impl<T> From<WriteOnly<T>> for DataSchemaBuilder
where
    T: Into<DataSchemaBuilder>,
{
    fn from(data_schema: WriteOnly<T>) -> Self {
        let DataSchemaBuilder { mut partial, info } = data_schema.inner.into();
        partial.write_only = true;

        Self { partial, info }
    }
}

impl<T> From<ReadOnly<T>> for PartialDataSchemaBuilder
where
    T: Into<PartialDataSchemaBuilder>,
{
    fn from(data_schema: ReadOnly<T>) -> Self {
        let mut data_schema = data_schema.inner.into();
        data_schema.read_only = true;
        data_schema
    }
}

impl<T> From<WriteOnly<T>> for PartialDataSchemaBuilder
where
    T: Into<PartialDataSchemaBuilder>,
{
    fn from(data_schema: WriteOnly<T>) -> Self {
        let mut data_schema = data_schema.inner.into();
        data_schema.write_only = true;
        data_schema
    }
}

impl From<StatelessDataSchemaType> for DataSchemaSubtype {
    fn from(ty: StatelessDataSchemaType) -> Self {
        match ty {
            StatelessDataSchemaType::Boolean => DataSchemaSubtype::Boolean,
            StatelessDataSchemaType::Null => DataSchemaSubtype::Null,
        }
    }
}

impl<T> From<StatelessDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
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
        }
    }
}

impl<T> From<StatelessDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
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
        }
    }
}

impl<T> From<ArrayDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
{
    fn from(builder: ArrayDataSchemaBuilder<T>) -> Self {
        let ArrayDataSchemaBuilder {
            inner,
            items,
            min_items,
            max_items,
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
        }
    }
}

impl<T> From<ArrayDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
{
    fn from(builder: ArrayDataSchemaBuilder<T>) -> Self {
        let ArrayDataSchemaBuilder {
            inner,
            items,
            min_items,
            max_items,
        } = builder;
        let PartialDataSchemaBuilder {
            constant: _,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
        } = inner.into();

        let items = items.is_empty().not().then(|| items);
        let subtype = Some(DataSchemaSubtype::Array(ArraySchema {
            items,
            min_items,
            max_items,
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
        }
    }
}

impl<T> From<NumberDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
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
        }
    }
}

impl<T> From<NumberDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
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
        }
    }
}

impl<T> From<IntegerDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
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
        }
    }
}

impl<T> From<IntegerDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
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
        }
    }
}

impl<T> From<ObjectDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
{
    fn from(builder: ObjectDataSchemaBuilder<T>) -> Self {
        let ObjectDataSchemaBuilder {
            inner,
            properties,
            required,
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
        }
    }
}

impl<T> From<ObjectDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
{
    fn from(builder: ObjectDataSchemaBuilder<T>) -> Self {
        let ObjectDataSchemaBuilder {
            inner,
            properties,
            required,
        } = builder;
        let PartialDataSchemaBuilder {
            constant: _,
            unit,
            one_of: _,
            enumeration: _,
            read_only,
            write_only,
            format,
        } = inner.into();

        let properties = properties
            .is_empty()
            .not()
            .then(|| properties.into_iter().collect());
        let required = required.is_empty().not().then(|| required);
        let subtype = Some(DataSchemaSubtype::Object(ObjectSchema {
            properties,
            required,
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
        }
    }
}

impl<T> From<StringDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
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
        }
    }
}

impl<T> From<StringDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
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
        }
    }
}

impl<T> From<ReadOnly<T>> for DataSchema
where
    T: Into<DataSchema>,
{
    fn from(builder: ReadOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: true,
            ..data_schema
        }
    }
}

impl<T> From<WriteOnly<T>> for DataSchema
where
    T: Into<DataSchema>,
{
    fn from(builder: WriteOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: false,
            ..data_schema
        }
    }
}

impl<T> From<ReadOnly<T>> for PartialDataSchema
where
    T: Into<PartialDataSchema>,
{
    fn from(builder: ReadOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: true,
            ..data_schema
        }
    }
}

impl<T> From<WriteOnly<T>> for PartialDataSchema
where
    T: Into<PartialDataSchema>,
{
    fn from(builder: WriteOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: false,
            ..data_schema
        }
    }
}

impl<T> From<EnumDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
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
        }
    }
}

impl<T> From<EnumDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
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
        }
    }
}

impl<T> From<OneOfDataSchemaBuilder<T>> for DataSchema
where
    T: Into<DataSchemaBuilder>,
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
        }
    }
}

impl<T> From<OneOfDataSchemaBuilder<T>> for PartialDataSchema
where
    T: Into<PartialDataSchemaBuilder>,
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
        }
    }
}

pub(super) trait CheckableDataSchema {
    fn check(&self) -> Result<(), Error>;
}

impl CheckableDataSchema for DataSchema {
    fn check(&self) -> Result<(), Error> {
        check_data_schema_subtype(&self.subtype)?;
        check_one_of_schema(self.one_of.as_deref())?;
        Ok(())
    }
}

impl CheckableDataSchema for PartialDataSchema {
    fn check(&self) -> Result<(), Error> {
        check_data_schema_subtype(&self.subtype)?;
        check_one_of_schema(self.one_of.as_deref())?;
        Ok(())
    }
}

pub(super) fn check_data_schema_subtype(
    mut subtype: &Option<DataSchemaSubtype>,
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
    use serde_json::json;

    use crate::thing::{ArraySchema, ObjectSchema};

    use super::*;

    #[test]
    fn null_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default().null().into();
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
            }
        );
    }

    #[test]
    fn null_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default().null().into();
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
            }
        );
    }

    #[test]
    fn boolean_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default().bool().into();
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
            }
        );
    }

    #[test]
    fn boolean_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default().bool().into();
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
            }
        );
    }

    #[test]
    fn string_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default().string().into();
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
            }
        );
    }

    #[test]
    fn string_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default().string().into();
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
            }
        );
    }

    #[test]
    fn empty_simple_array() {
        let data_schema: DataSchema = DataSchemaBuilder::default().array().into();
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
                })),
            }
        );
    }

    #[test]
    fn empty_partial_array() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default().array().into();
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
                })),
            }
        );
    }

    #[test]
    fn number_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default().number().into();
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
            }
        );
    }
    #[test]
    fn number_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default().number().into();
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
            }
        );
    }

    #[test]
    fn integer_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default().integer().into();
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
            }
        );
    }

    #[test]
    fn partial_simple() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default().integer().into();
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
            }
        );
    }

    #[test]
    fn empty_simple_object() {
        let data_schema: DataSchema = DataSchemaBuilder::default().object().into();
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
                    required: None
                })),
            }
        );
    }

    #[test]
    fn empty_partial_object() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default().object().into();
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
                    required: None
                })),
            }
        );
    }

    #[test]
    fn constant_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
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
            }
        );
    }

    #[test]
    fn constant_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default()
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
            }
        );
    }

    #[test]
    fn enum_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
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
            }
        );
    }

    #[test]
    fn enum_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default()
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
            }
        );
    }

    #[test]
    fn read_only_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default().bool().read_only().into();
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
            }
        );
    }

    #[test]
    fn read_only_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default()
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
            }
        );
    }

    #[test]
    fn read_only_to_builder() {
        let data_schema_builder: DataSchemaBuilder = ReadOnly {
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
                    format: None
                },
                info: Default::default(),
            }
        );
    }

    #[test]
    fn read_only_to_partial_builder() {
        let data_schema_builder: PartialDataSchemaBuilder = ReadOnly {
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
                format: None
            },
        );
    }

    #[test]
    fn write_only_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default().bool().write_only().into();
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
            }
        );
    }

    #[test]
    fn write_only_partial() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default()
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
            }
        );
    }

    #[test]
    fn write_only_to_builder() {
        let data_schema_builder: DataSchemaBuilder = WriteOnly {
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
                    format: None
                },
                info: Default::default(),
            }
        );
    }

    #[test]
    fn write_only_to_partial_builder() {
        let data_schema_builder: PartialDataSchemaBuilder = WriteOnly {
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
                format: None
            },
        );
    }

    #[test]
    fn null_full() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .null()
            .attype("attype1")
            .attype("attype2")
            .title("title")
            .titles(|b| b.add("en", "title_en").add("it", "title_it"))
            .description("description")
            .descriptions(|b| b.add("en", "description_en").add("it", "description_it"))
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
            }
        );
    }

    #[test]
    fn enum_full() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .enumeration("variant1")
            .enumeration("variant2")
            .enumeration(3u32)
            .attype("attype")
            .title("title")
            .titles(|b| b.add("en", "title_en").add("it", "title_it"))
            .description("description")
            .descriptions(|b| b.add("en", "description_en").add("it", "description_it"))
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
            }
        );
    }

    #[test]
    fn read_only_enum() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
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
            }
        );
    }

    #[test]
    fn array_with_content() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .array()
            .min_items(0)
            .max_items(5)
            .append(|b| b.constant("hello"))
            .append(|b| b.bool())
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
                            subtype: None
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
                            subtype: Some(DataSchemaSubtype::Boolean)
                        },
                    ]),
                    min_items: Some(0),
                    max_items: Some(5),
                })),
            }
        );
    }

    #[test]
    fn array_partial_with_content() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default()
            .array()
            .min_items(0)
            .max_items(5)
            .append(|b| b.constant("hello"))
            .append(|b| b.bool())
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
                            subtype: None
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
                            subtype: Some(DataSchemaSubtype::Boolean)
                        },
                    ]),
                    min_items: Some(0),
                    max_items: Some(5),
                })),
            }
        );
    }

    #[test]
    fn object_with_content() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .object()
            .property("hello", false, |b| b.bool())
            .property("world", true, |b| b.title("title").number())
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
                                }
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                    required: Some(vec!["world".to_string()])
                })),
            }
        );
    }

    #[test]
    fn object_partial_with_content() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default()
            .object()
            .property("hello", false, |b| b.bool())
            .property("world", true, |b| b.title("title").number())
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
                                }
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                    required: Some(vec!["world".to_string()])
                })),
            }
        );
    }

    #[test]
    fn integer_with_data() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
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
                }))
            },
        );
    }

    #[test]
    fn number_with_data() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
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
                }))
            },
        );
    }

    #[test]
    fn string_with_data() {
        let data_schema: DataSchema = DataSchemaBuilder::default().string().max_length(32).into();
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
                }))
            },
        );
    }

    #[test]
    fn one_of_simple() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| b.number())
            .one_of(|b| b.integer())
            .one_of(|b| b.string())
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
                    },
                ]),
                enumeration: None,
                read_only: false,
                write_only: false,
                format: None,
                subtype: None,
            },
        );
    }

    #[test]
    fn one_of_nested() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .object()
            .property("hello", true, |b| {
                b.one_of(|b| b.string()).one_of(|b| b.integer())
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
                                    },
                                ]),
                                enumeration: None,
                                read_only: false,
                                write_only: false,
                                format: None,
                                subtype: None,
                            }
                        ),]
                        .into_iter()
                        .collect()
                    ),
                    required: Some(vec!["hello".to_string()])
                })),
            },
        );
    }

    #[test]
    fn check_valid_data_schema() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.).multiple_of(2.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.).multiple_of(7.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert!(data_schema.check().is_ok());
    }

    #[test]
    fn check_invalid_data_schema() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(5)
                    .max_items(2)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(5.).maximum(0.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(10).maximum(5))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(42.).maximum(20.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(f64::NAN))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::NanMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(f64::NAN).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::NanMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(20).maximum(10))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
                    .property("b", false, |b| b.integer().minimum(20).maximum(10))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .one_of(|b| b.one_of(|b| b.number().minimum(20.).maximum(10.)))
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.one_of(|b| b.number().minimum(5.).maximum(0.)))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMinMax);
    }

    #[test]
    fn check_invalid_data_schema_multiple_of() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .array()
            .append(|b| b.number().multiple_of(0.))
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMultipleOf);

        let data_schema: DataSchema = DataSchemaBuilder::default()
            .array()
            .append(|b| b.number().multiple_of(-2.))
            .into();

        assert_eq!(data_schema.check().unwrap_err(), Error::InvalidMultipleOf);
    }

    #[test]
    fn check_valid_partial_data_schema() {
        let data_schema: PartialDataSchema = PartialDataSchemaBuilder::default()
            .one_of(|b| {
                b.array()
                    .min_items(2)
                    .max_items(5)
                    .append(|b| b.number().minimum(0.).maximum(5.).multiple_of(2.))
                    .append(|b| b.integer().minimum(5).maximum(10))
            })
            .one_of(|b| b.number().minimum(20.).maximum(42.).multiple_of(3.))
            .one_of(|b| {
                b.object()
                    .property("a", false, |b| b.integer().minimum(10).maximum(20))
            })
            .into();

        assert!(data_schema.check().is_ok());
    }
}
