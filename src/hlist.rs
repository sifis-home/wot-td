use std::fmt;

use serde::{
    de::Visitor,
    ser::{SerializeStruct, SerializeTuple},
    Deserialize, Serialize, Serializer,
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Nil;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Cons<T, U = Nil> {
    pub(crate) head: T,
    pub(crate) tail: U,
}

pub(crate) trait HList: Sized {
    const SIZE: usize;
}

impl HList for Nil {
    const SIZE: usize = 0;
}

impl<T, U> HList for Cons<T, U>
where
    U: HList,
{
    const SIZE: usize = U::SIZE + 1;
}

impl<T> Cons<T, Nil> {
    #[inline]
    pub(crate) fn new_head(head: T) -> Self {
        Cons { head, tail: Nil }
    }

    #[inline]
    pub(crate) fn add<U>(self, value: U) -> Cons<T, Cons<U, Nil>> {
        let Self { head, tail: Nil } = self;

        Cons {
            head,
            tail: Cons {
                head: value,
                tail: Nil,
            },
        }
    }
}

impl Serialize for Nil {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_struct("Nil", 0)?.end()
    }
}

impl<'de> Deserialize<'de> for Nil {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Helper;

        impl<'de> Visitor<'de> for Helper {
            type Value = Nil;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Nil")
            }
        }

        deserializer.deserialize_unit(Helper)
    }
}

struct SerializeVisitor<L> {
    hlist: L,
}

trait SerializeVisitorHList {
    fn visit<S: Serializer>(self, tuple: S::SerializeTuple) -> Result<S::Ok, S::Error>;
}

impl<'a, T, U> SerializeVisitorHList for SerializeVisitor<&'a Cons<T, U>>
where
    Cons<T, U>: HList,
    T: Serialize,
    SerializeVisitor<&'a U>: SerializeVisitorHList,
{
    fn visit<S>(self, mut tuple: S::SerializeTuple) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let Self {
            hlist: Cons { head, tail },
        } = self;

        tuple.serialize_element(head)?;
        SerializeVisitor { hlist: tail }.visit::<S>(tuple)
    }
}

impl SerializeVisitorHList for SerializeVisitor<&'_ Nil> {
    fn visit<S>(self, tuple: S::SerializeTuple) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        tuple.end()
    }
}

impl<T, U> Serialize for Cons<T, U>
where
    T: Serialize,
    Self: HList,
    for<'a> SerializeVisitor<&'a Cons<T, U>>: SerializeVisitorHList,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let tuple = serializer.serialize_tuple(Self::SIZE)?;
        SerializeVisitor { hlist: self }.visit::<S>(tuple)
    }
}

#[cfg(test)]
mod tests {
    use serde_json::Value;

    use super::*;

    #[test]
    fn serialize_flatten_nil() {
        #[derive(Debug, Serialize)]
        struct A {
            a: i32,
            #[serde(flatten)]
            b: Nil,
        }

        let value = serde_json::to_value(A { a: 42, b: Nil }).unwrap();
        dbg!(&value);
        assert_eq!(value.get("a").unwrap(), &Value::Number(42.into()));
        assert!(value.get("b").is_none());
    }

    #[test]
    fn serialize_unit_cons() {
        #[derive(Debug, Serialize)]
        struct A {
            a: i32,
            b: Cons<i32, Cons<&'static str>>,
        }

        let value = serde_json::to_value(A {
            a: 42,
            b: Cons::new_head(17).add("17"),
        })
        .unwrap();
        dbg!(&value);
        assert_eq!(value.get("a").unwrap(), &Value::Number(42.into()));
        assert_eq!(
            value.get("b").unwrap(),
            &Value::Array(vec![Value::Number(17.into()), Value::String("17".into())])
        );
    }
}
