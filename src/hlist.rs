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

#[macro_use]
mod macros {
    /// Assemble a lifo hlist type out of a list of type
    #[macro_export]
    macro_rules! make_hlist {
        {type $id:ident = [$first:ty, $($next:ty),+];} => {
          $crate::make_hlist!(type $id = [$($next),+] => Cons<$first, Nil>);
        };

        {type $id:ident = [$first:ty, $($next:ty),+] => $prev:ty } => {
          $crate::make_hlist!(type $id = [$($next),+] => Cons<$first, $prev>);
        };

        {type $id:ident = [$first:ty] => $prev:ty} => {
            type $id = Cons<$first, $prev>;
        };

        {type $id:ident = [$first:ty];} => {
            type $id = Cons<$first, Nil>;
        };
    }
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
}

impl<T, U> Cons<T, U> {
    #[inline]
    pub(crate) fn add<V>(self, value: V) -> Cons<V, Self> {
        Cons {
            head: value,
            tail: self,
        }
    }

    pub(crate) fn next(&self) -> &U {
        &self.tail
    }

    pub(crate) fn next_mut(&mut self) -> &mut U {
        &mut self.tail
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

#[cfg(test)]
mod tests {
    use serde_json::{json, Value};

    use super::*;

    #[test]
    fn chain() {
        let v = Cons::new_head("A".to_string())
            .add("B".to_string())
            .add("C".to_string());

        dbg!(&v);
    }

    #[test]
    fn edit() {
        let mut e = Cons::new_head("A".to_string())
            .add("B".to_string())
            .add("C".to_string());

        e.next_mut().head.push('a');
        e.next_mut().next_mut().head.push('b');

        dbg!(&e);
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
            b: Cons<C, Cons<B, Nil>>,
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
            b: Cons<Cons<B, C>, Nil>,
        }

        let v = json!({
            "a": 42,
            "foo": 42,
            "bar": "42",
        });

        let a: A = serde_json::from_value(v).unwrap();

        dbg!(&a);

        assert_eq!(a.a, 42);
        assert_eq!(a.b.head.head.foo, 42);
        assert_eq!(a.b.head.tail.bar, String::from("42"));
    }
}
