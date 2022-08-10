use serde::{ser::SerializeStruct, Deserialize, Deserializer, Serialize};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Nil;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Cons<T, U = Nil> {
    #[serde(flatten)]
    pub(crate) head: T,
    #[serde(flatten)]
    pub(crate) tail: U,
}

impl<T> Cons<T, Nil> {
    #[inline]
    pub(crate) fn new_head(head: T) -> Self {
        Cons { head, tail: Nil {} }
    }
}

impl<T, U> Cons<T, U> {
    #[inline]
    pub(crate) fn add<V>(self, value: V) -> Cons<V, Self> {
        Cons {
            head: value,
            tail: self,
        }
    }

    /// Split the head element of the heterogenous list
    pub fn split_head(self) -> (T, U) {
        let Cons { head, tail } = self;

        (head, tail)
    }
}

pub trait HListRef {
    type Target;

    fn to_ref(self) -> Self::Target;
}

impl<'a, T, U> HListRef for &'a Cons<T, U>
where
    &'a U: HListRef,
{
    type Target = Cons<&'a T, <&'a U as HListRef>::Target>;

    #[inline]
    fn to_ref(self) -> Self::Target {
        let Cons { head, tail } = self;
        Cons {
            head,
            tail: tail.to_ref(),
        }
    }
}

impl<'a> HListRef for &'a Nil {
    type Target = Nil;

    #[inline]
    fn to_ref(self) -> Self::Target {
        Nil
    }
}

pub trait HListMut {
    type Target;

    // This is ignored because `HListMut` must be implemented for mutable references only,
    // therefore `to_mut` must take `self`. The reason behind this design is because we don't have
    // GATs on stable in order to write something like this:
    // ```
    // trait HList {
    //     type ToRef<'a> where Self: 'a;
    //     type ToMut<'a> where Self: 'a;
    //
    //     fn to_ref(&self) -> Self::ToRef<'_>;
    //     fn to_mut(&mut self) -> Self::ToMut<'_>;
    // }
    // ```
    #[allow(clippy::wrong_self_convention)]
    fn to_mut(self) -> Self::Target;
}

impl<'a, T, U> HListMut for &'a mut Cons<T, U>
where
    &'a mut U: HListMut,
{
    type Target = Cons<&'a mut T, <&'a mut U as HListMut>::Target>;

    #[inline]
    fn to_mut(self) -> Self::Target {
        let Cons { head, tail } = self;
        Cons {
            head,
            tail: tail.to_mut(),
        }
    }
}

impl<'a> HListMut for &'a mut Nil {
    type Target = Nil;

    #[inline]
    fn to_mut(self) -> Self::Target {
        Nil
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
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct NilStruct {}

        NilStruct::deserialize(deserializer)?;
        Ok(Nil)
    }
}

impl From<()> for Nil {
    fn from(_: ()) -> Self {
        Nil
    }
}

impl From<Nil> for () {
    fn from(_: Nil) -> Self {}
}

#[cfg(test)]
mod tests {
    use serde_json::{json, Value};

    use super::*;

    #[test]
    fn split_head() {
        #[derive(Debug, PartialEq)]
        struct A(i32);

        #[derive(Debug, PartialEq)]
        struct B(f32);

        #[derive(Debug, PartialEq)]
        struct C(String);

        let list = Cons::new_head(A(42)).add(B(1.234)).add(C("C".to_string()));

        assert_eq!(
            list.split_head(),
            (C("C".to_string()), Cons::new_head(A(42)).add(B(1.234))),
        )
    }

    #[test]
    fn chain() {
        let list = Cons::new_head("A").add(2).add("C".to_string());

        assert_eq!(
            list,
            Cons {
                head: "C".to_string(),
                tail: Cons {
                    head: 2,
                    tail: Cons {
                        head: "A",
                        tail: Nil,
                    },
                }
            }
        );
    }

    #[test]
    fn serialize_flatten_nil() {
        #[derive(Debug, Serialize)]
        struct A {
            a: i32,
            #[serde(flatten)]
            b: Nil,
        }

        let value = serde_json::to_value(A { a: 42, b: Nil {} }).unwrap();
        assert_eq!(value.get("a").unwrap(), &Value::Number(42.into()));
        assert!(value.get("b").is_none());
    }

    #[test]
    fn serialize_cons() {
        #[derive(Debug, Serialize)]
        struct C {
            bar: &'static str,
        }
        #[derive(Debug, Serialize)]
        struct B {
            foo: usize,
        }
        #[derive(Debug, Serialize)]
        struct A {
            a: i32,
            #[serde(flatten)]
            b: Cons<C, Cons<B, Nil>>,
        }

        let value = serde_json::to_value(A {
            a: 42,
            b: Cons::new_head(B { foo: 42 }).add(C { bar: "42" }),
        })
        .unwrap();
        assert_eq!(value.get("a").unwrap(), &Value::Number(42.into()));
        assert_eq!(value.get("foo").unwrap(), &Value::Number(42.into()));
    }

    #[test]
    fn deserialize_cons() {
        #[derive(Debug, Deserialize)]
        struct C {
            bar: String,
        }
        #[derive(Debug, Deserialize)]
        struct B {
            foo: usize,
        }
        #[derive(Debug, Deserialize)]
        struct A {
            a: i32,
            #[serde(flatten)]
            b: Cons<Cons<B, C>, Nil>,
        }

        let v = json!({
            "a": 42,
            "foo": 42,
            "bar": "42",
        });

        let a: A = serde_json::from_value(v).unwrap();

        assert_eq!(a.a, 42);
        assert_eq!(a.b.head.head.foo, 42);
        assert_eq!(a.b.head.tail.bar, String::from("42"));
    }

    #[test]
    fn to_ref() {
        #[derive(Debug, PartialEq)]
        struct A(i32);

        #[derive(Debug, PartialEq)]
        struct B(f32);

        #[derive(Debug, PartialEq)]
        struct C(String);

        let list = Cons::new_head(A(42))
            .add(B(1.234))
            .add(C("hello".to_string()));

        assert_eq!(
            list.to_ref(),
            Cons::new_head(&A(42))
                .add(&B(1.234))
                .add(&C("hello".to_string())),
        )
    }

    #[test]
    fn to_mut() {
        #[derive(Debug, PartialEq)]
        struct A(i32);

        #[derive(Debug, PartialEq)]
        struct B(f32);

        #[derive(Debug, PartialEq)]
        struct C(String);

        let mut list = Cons::new_head(A(42))
            .add(B(1.234))
            .add(C("hello".to_string()));

        assert_eq!(
            list.to_mut(),
            Cons::new_head(&mut A(42))
                .add(&mut B(1.234))
                .add(&mut C("hello".to_string())),
        )
    }
}
