use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Nil {}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Cons<T, U = Nil> {
    #[serde(flatten)]
    pub(crate) head: T,
    #[serde(flatten)]
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
        Cons { head, tail: Nil {} }
    }

    #[inline]
    pub(crate) fn add<U>(self, value: U) -> Cons<T, Cons<U, Nil>> {
        let Self { head, tail: Nil {} } = self;

        Cons {
            head,
            tail: Cons {
                head: value,
                tail: Nil {},
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::{json, Value};

    use super::*;

    #[test]
    fn serialize_flatten_nil() {
        #[derive(Debug, Serialize)]
        struct A {
            a: i32,
            #[serde(flatten)]
            b: Nil,
        }

        let value = serde_json::to_value(A { a: 42, b: Nil {} }).unwrap();
        dbg!(&value);
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
            b: Cons<B, Cons<C>>,
        }

        let value = serde_json::to_value(A {
            a: 42,
            b: Cons::new_head(B { foo: 42 }).add(C { bar: "42" }),
        })
        .unwrap();
        dbg!(&value);
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
            b: Cons<B, Cons<C>>,
        }

        let v = json!({
            "a": 42,
            "foo": 42,
            "bar": "42",
        });

        let a: A = serde_json::from_value(v).unwrap();

        dbg!(&a);

        assert_eq!(a.a, 42);
        assert_eq!(a.b.head.foo, 42);
        assert_eq!(a.b.tail.head.bar, String::from("42"));
    }
}
