use crate::thing::{
    ArraySchema, DataSchema, DataSchemaSubtype, IntegerSchema, NumberSchema, ObjectSchema,
};

use super::*;

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
#[derive(Default)]
pub struct DataSchemaBuilder {
    attype: Option<String>,
    title: Option<String>,
    titles: Option<MultiLanguage>,
    description: Option<String>,
    descriptions: Option<MultiLanguage>,
    constant: Option<Value>,
    unit: Option<String>,
    one_of: Vec<DataSchema>,
    enumeration: Vec<Value>,
    read_only: bool,
    write_only: bool,
    format: Option<String>,
}

pub trait BuildableDataSchema: Sized {
    fn attype(self, value: impl Into<String>) -> Self;
    fn title(self, value: impl Into<String>) -> Self;
    fn titles<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>;
    fn description(self, value: impl Into<String>) -> Self;
    fn descriptions<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>;
    fn unit(self, value: impl Into<String>) -> Self;
    fn format(self, value: impl Into<String>) -> Self;
}

pub trait SpecializableDataSchema: BuildableDataSchema {
    fn array(self) -> ArrayDataSchemaBuilder;
    fn bool(self) -> StatelessDataSchemaBuilder;
    fn number(self) -> NumberDataSchemaBuilder;
    fn integer(self) -> IntegerDataSchemaBuilder;
    fn object(self) -> ObjectDataSchemaBuilder;
    fn string(self) -> StatelessDataSchemaBuilder;
    fn null(self) -> StatelessDataSchemaBuilder;
    fn constant(self, value: impl Into<Value>) -> ReadOnly<StatelessDataSchemaBuilder>;
}

pub trait EnumerableDataSchema {
    fn enumeration(self, value: impl Into<Value>) -> EnumDataSchemaBuilder;
}

pub trait UnionDataSchema {
    fn one_of<F, T>(self, f: F) -> OneOfDataSchemaBuilder
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>;
}

pub trait ReadableWriteableDataSchema: BuildableDataSchema {
    fn read_only(self) -> ReadOnly<Self>;
    fn write_only(self) -> WriteOnly<Self>;
}

pub struct ArrayDataSchemaBuilder {
    inner: DataSchemaBuilder,
    items: Vec<DataSchema>,
    min_items: Option<u32>,
    max_items: Option<u32>,
}

pub struct NumberDataSchemaBuilder {
    inner: DataSchemaBuilder,
    maximum: Option<f64>,
    minimum: Option<f64>,
}

pub struct IntegerDataSchemaBuilder {
    inner: DataSchemaBuilder,
    maximum: Option<usize>,
    minimum: Option<usize>,
}

pub struct ObjectDataSchemaBuilder {
    inner: DataSchemaBuilder,
    properties: Vec<(String, DataSchema)>,
    required: Vec<String>,
}

pub struct EnumDataSchemaBuilder {
    inner: DataSchemaBuilder,
}

pub struct OneOfDataSchemaBuilder {
    inner: DataSchemaBuilder,
}

pub enum StatelessDataSchemaType {
    Boolean,
    Null,
    String,
}

pub struct StatelessDataSchemaBuilder {
    inner: DataSchemaBuilder,
    ty: Option<StatelessDataSchemaType>,
}

pub struct ReadOnly<Inner> {
    inner: Inner,
}

pub struct WriteOnly<Inner> {
    inner: Inner,
}

macro_rules! opt_field_builder {
    ($($field:ident : $ty:ty),* $(,)?) => {
        $(
            pub fn $field(mut self, value: $ty) -> Self {
                self.$field = Some(value);
                self
            }
        )*
    };
}

impl ArrayDataSchemaBuilder {
    opt_field_builder!(min_items: u32, max_items: u32);

    pub fn append<F, T>(mut self, f: F) -> Self
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        self.items.push(f(DataSchemaBuilder::default()).into());
        self
    }
}

impl NumberDataSchemaBuilder {
    opt_field_builder!(minimum: f64, maximum: f64);
}

impl IntegerDataSchemaBuilder {
    opt_field_builder!(minimum: usize, maximum: usize);
}

impl ObjectDataSchemaBuilder {
    pub fn property<F, T>(mut self, name: impl Into<String>, required: bool, f: F) -> Self
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

macro_rules! buildable_data_schema_delegate {
    ($self:ident . $field:ident -> $fn:ident($($arg:ident),*)) => {{
        $self.$field = $self.$field.$fn($($arg),*);
        $self
    }};
}

macro_rules! impl_delegate_buildable_data_schema {
    () => {};

    ($kind:ident $(<$($ty:ident),+>)? : $inner:ident $(, $($rest:tt)*)?) => {
        impl $(<$($ty),+>)? BuildableDataSchema for $kind $(<$($ty),+>)?
        $(
            where
                $($ty: BuildableDataSchema),+
        )?
        {
            #[inline]
            fn attype(mut self, value: impl Into<String>) -> Self {
                buildable_data_schema_delegate!(self.$inner -> attype(value))
            }

            #[inline]
            fn title(mut self, value: impl Into<String>) -> Self {
                buildable_data_schema_delegate!(self.$inner -> title(value))
            }

            #[inline]
            fn titles<F>(mut self, f: F) -> Self
            where
                F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
            {
                buildable_data_schema_delegate!(self.$inner -> titles(f))
            }

            #[inline]
            fn description(mut self, value: impl Into<String>) -> Self {
                buildable_data_schema_delegate!(self.$inner -> description(value))
            }

            #[inline]
            fn descriptions<F>(mut self, f: F) -> Self
            where
                F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
            {
                buildable_data_schema_delegate!(self.$inner -> descriptions(f))
            }

            #[inline]
            fn unit(mut self, value: impl Into<String>) -> Self {
                buildable_data_schema_delegate!(self.$inner -> unit(value))
            }

            #[inline]
            fn format(mut self, value: impl Into<String>) -> Self {
                buildable_data_schema_delegate!(self.$inner -> format(value))
            }
        }

        $(
            impl_delegate_buildable_data_schema!($($rest)*);
        )?
    };

    ($kind:ident $(<$($ty:ident),+>)? $(, $($rest:tt)*)? ) => {
        impl_delegate_buildable_data_schema!($kind $(<$($ty),+>)?: inner $(, $($rest)*)?);
    };
}

impl_delegate_buildable_data_schema!(
    ArrayDataSchemaBuilder,
    NumberDataSchemaBuilder,
    IntegerDataSchemaBuilder,
    ObjectDataSchemaBuilder,
    StatelessDataSchemaBuilder,
    ReadOnly<Inner>,
    WriteOnly<Inner>,
    EnumDataSchemaBuilder,
    OneOfDataSchemaBuilder,
);

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

impl BuildableDataSchema for DataSchemaBuilder {
    trait_opt_field_builder!(
        attype: String,
        title: String,
        description: String,
        unit: String,
        format: String,
    );

    fn titles<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);
        self.titles = Some(builder.values);
        self
    }

    fn descriptions<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut MultiLanguageBuilder<String>) -> &mut MultiLanguageBuilder<String>,
    {
        let mut builder = MultiLanguageBuilder::default();
        f(&mut builder);
        self.descriptions = Some(builder.values);
        self
    }
}

impl SpecializableDataSchema for DataSchemaBuilder {
    fn array(self) -> ArrayDataSchemaBuilder {
        ArrayDataSchemaBuilder {
            inner: self,
            items: Default::default(),
            min_items: Default::default(),
            max_items: Default::default(),
        }
    }

    fn bool(self) -> StatelessDataSchemaBuilder {
        StatelessDataSchemaBuilder {
            inner: self,
            ty: Some(StatelessDataSchemaType::Boolean),
        }
    }

    fn number(self) -> NumberDataSchemaBuilder {
        NumberDataSchemaBuilder {
            inner: self,
            maximum: Default::default(),
            minimum: Default::default(),
        }
    }

    fn integer(self) -> IntegerDataSchemaBuilder {
        IntegerDataSchemaBuilder {
            inner: self,
            maximum: Default::default(),
            minimum: Default::default(),
        }
    }

    fn object(self) -> ObjectDataSchemaBuilder {
        ObjectDataSchemaBuilder {
            inner: self,
            properties: Default::default(),
            required: Default::default(),
        }
    }

    fn string(self) -> StatelessDataSchemaBuilder {
        StatelessDataSchemaBuilder {
            inner: self,
            ty: Some(StatelessDataSchemaType::String),
        }
    }

    fn null(self) -> StatelessDataSchemaBuilder {
        StatelessDataSchemaBuilder {
            inner: self,
            ty: Some(StatelessDataSchemaType::Null),
        }
    }

    fn constant(mut self, value: impl Into<Value>) -> ReadOnly<StatelessDataSchemaBuilder> {
        self.constant = Some(value.into());
        ReadOnly {
            inner: StatelessDataSchemaBuilder {
                inner: self,
                ty: None,
            },
        }
    }
}

macro_rules! impl_enumerable_data_schema {
    () => {};

    ($kind:ident $(<$($ty:ident),+>)? : $inner:ident $(, $($rest:tt)*)?) => {
        impl $(<$($ty),+>)? EnumerableDataSchema for $kind $(<$($ty),+>)?
        $(
            where
                $($ty: EnumerableDataSchema),+
        )?
        {
            #[inline]
            fn enumeration(self, value: impl Into<Value>) -> EnumDataSchemaBuilder {
                self.$inner.enumeration(value)
            }
        }

        $(
            impl_enumerable_data_schema!($($rest)*);
        )?
    };

    ($kind:ident $(<$($ty:ident),+>)? $(, $($rest:tt)*)? ) => {
        impl_enumerable_data_schema!($kind $(<$($ty),+>)?: inner $(, $($rest)*)?);
    };
}

impl_enumerable_data_schema!(ReadOnly<Inner>, WriteOnly<Inner>, EnumDataSchemaBuilder);

impl EnumerableDataSchema for DataSchemaBuilder {
    fn enumeration(mut self, value: impl Into<Value>) -> EnumDataSchemaBuilder {
        self.enumeration.push(value.into());
        EnumDataSchemaBuilder { inner: self }
    }
}

macro_rules! impl_union_data_schema {
    () => {};

    ($kind:ident $(<$($ty:ident),+>)? : $inner:ident $(, $($rest:tt)*)?) => {
        impl $(<$($ty),+>)? UnionDataSchema for $kind $(<$($ty),+>)?
        $(
            where
                $($ty: UnionDataSchema),+
        )?
        {
            #[inline]
            fn one_of<F, T>(self, f: F) -> OneOfDataSchemaBuilder
                where
                    F: FnOnce(DataSchemaBuilder) -> T,
                    T: Into<DataSchema>,
            {
                self.$inner.one_of(f)
            }
        }

        $(
            impl_union_data_schema!($($rest)*);
        )?
    };

    ($kind:ident $(<$($ty:ident),+>)? $(, $($rest:tt)*)? ) => {
        impl_union_data_schema!($kind $(<$($ty),+>)?: inner $(, $($rest)*)?);
    };
}

impl_union_data_schema!(ReadOnly<Inner>, WriteOnly<Inner>, OneOfDataSchemaBuilder);

impl UnionDataSchema for DataSchemaBuilder {
    fn one_of<F, T>(mut self, f: F) -> OneOfDataSchemaBuilder
    where
        F: FnOnce(DataSchemaBuilder) -> T,
        T: Into<DataSchema>,
    {
        self.one_of.push(f(DataSchemaBuilder::default()).into());
        OneOfDataSchemaBuilder { inner: self }
    }
}

macro_rules! impl_rw_data_schema {
    () => {};

    ($ty:ty $(, $($rest:tt)*)?) => {
        impl ReadableWriteableDataSchema for $ty
        {
            #[inline]
            fn read_only(mut self) -> ReadOnly<Self> {
                self.inner.read_only = true;
                ReadOnly {
                    inner: self,
                }
            }

            #[inline]
            fn write_only(mut self) -> WriteOnly<Self> {
                self.inner.write_only = true;
                WriteOnly {
                    inner: self,
                }
            }
        }

        $(
            impl_rw_data_schema!($($rest)*);
        )?
    };
}

impl_rw_data_schema!(
    StatelessDataSchemaBuilder,
    ArrayDataSchemaBuilder,
    NumberDataSchemaBuilder,
    IntegerDataSchemaBuilder,
    ObjectDataSchemaBuilder,
    EnumDataSchemaBuilder,
);

impl From<StatelessDataSchemaType> for DataSchemaSubtype {
    fn from(ty: StatelessDataSchemaType) -> Self {
        match ty {
            StatelessDataSchemaType::Boolean => DataSchemaSubtype::Boolean,
            StatelessDataSchemaType::Null => DataSchemaSubtype::Null,
            StatelessDataSchemaType::String => DataSchemaSubtype::String,
        }
    }
}

impl From<StatelessDataSchemaBuilder> for DataSchema {
    fn from(builder: StatelessDataSchemaBuilder) -> Self {
        let StatelessDataSchemaBuilder {
            inner:
                DataSchemaBuilder {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                    constant,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                },
            ty,
        } = builder;

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

impl From<ArrayDataSchemaBuilder> for DataSchema {
    fn from(builder: ArrayDataSchemaBuilder) -> Self {
        let ArrayDataSchemaBuilder {
            inner:
                DataSchemaBuilder {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                    constant: _,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                },
            items,
            min_items,
            max_items,
        } = builder;

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

impl From<NumberDataSchemaBuilder> for DataSchema {
    fn from(builder: NumberDataSchemaBuilder) -> Self {
        let NumberDataSchemaBuilder {
            inner:
                DataSchemaBuilder {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                    constant: _,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                },
            maximum,
            minimum,
        } = builder;

        let subtype = Some(DataSchemaSubtype::Number(NumberSchema { minimum, maximum }));

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

impl From<IntegerDataSchemaBuilder> for DataSchema {
    fn from(builder: IntegerDataSchemaBuilder) -> Self {
        let IntegerDataSchemaBuilder {
            inner:
                DataSchemaBuilder {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                    constant: _,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                },
            maximum,
            minimum,
        } = builder;

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

impl From<ObjectDataSchemaBuilder> for DataSchema {
    fn from(builder: ObjectDataSchemaBuilder) -> Self {
        let ObjectDataSchemaBuilder {
            inner:
                DataSchemaBuilder {
                    attype,
                    title,
                    titles,
                    description,
                    descriptions,
                    constant: _,
                    unit,
                    one_of: _,
                    enumeration: _,
                    read_only,
                    write_only,
                    format,
                },
            properties,
            required,
        } = builder;

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

impl<T> From<ReadOnly<T>> for DataSchema
where
    DataSchema: From<T>,
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
    DataSchema: From<T>,
{
    fn from(builder: WriteOnly<T>) -> Self {
        let data_schema = builder.inner.into();
        Self {
            read_only: false,
            ..data_schema
        }
    }
}

impl From<EnumDataSchemaBuilder> for DataSchema {
    fn from(builder: EnumDataSchemaBuilder) -> Self {
        let DataSchemaBuilder {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: _,
            unit,
            one_of: _,
            enumeration,
            read_only,
            write_only,
            format,
        } = builder.inner;

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

impl From<OneOfDataSchemaBuilder> for DataSchema {
    fn from(builder: OneOfDataSchemaBuilder) -> Self {
        let DataSchemaBuilder {
            attype,
            title,
            titles,
            description,
            descriptions,
            constant: _,
            unit,
            one_of,
            enumeration: _,
            read_only,
            write_only,
            format,
        } = builder.inner;

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
    fn string_simpl() {
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
                subtype: Some(DataSchemaSubtype::String),
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
                    minimum: None
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
    fn null_full() {
        let data_schema: DataSchema = DataSchemaBuilder::default()
            .null()
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
                attype: Some("attype".to_string()),
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
            .enumeration(3)
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
                attype: Some("attype".to_string()),
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
                        subtype: Some(DataSchemaSubtype::String),
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
                                        subtype: Some(DataSchemaSubtype::String),
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
}
