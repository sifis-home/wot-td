//! Heterogeneous List
//!
//! It is used for the internals of the extension system.

use serde::{ser::SerializeStruct, Deserialize, Deserializer, Serialize};

/// Empty type.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Nil;

/// List type.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Cons<T, U = Nil> {
    /// The _head_ of the list.
    ///
    /// This generally consists of some content appended to the heterogeneous list.
    #[serde(flatten)]
    pub head: T,

    /// The _tail_ of the list.
    ///
    /// This generally consists of `Nil` or `Cons` containing the rest of the items.
    #[serde(flatten)]
    pub tail: U,
}

impl Nil {
    /// Prepend a new `head` the heterogeneous list.
    #[inline]
    pub fn cons<T>(value: T) -> Cons<T, Nil> {
        Cons {
            head: value,
            tail: Nil,
        }
    }
}

impl<T, U> Cons<T, U> {
    /// Prepend a new `head` the heterogeneous list.
    #[inline]
    pub fn cons<V>(self, value: V) -> Cons<V, Self> {
        Cons {
            head: value,
            tail: self,
        }
    }

    /// Split the head element of the heterogeneous list.
    pub fn split_head(self) -> (T, U) {
        let Cons { head, tail } = self;

        (head, tail)
    }
}

// /// A conversion from an heterogenous list of values into an heterogenous list of references.
// pub trait HListRef {
//     /// The heterogenous list of references.
//     type Target<'a>
//     where
//         Self: 'a;
//
//     /// Create a heterogeneous list of references.
//     fn to_ref(&self) -> Self::Target<'_>;
// }

// impl<T, U> HListRef for Cons<T, U>
// where
//     U: HListRef,
// {
//     type Target<'a> = Cons<&'a T, <U as HListRef>::Target<'a>>
//     where
//         Self: 'a;
//
//     #[inline]
//     fn to_ref(&self) -> Self::Target<'_> {
//         let Cons { head, tail } = self;
//         Cons {
//             head,
//             tail: tail.to_ref(),
//         }
//     }
// }
//
// impl HListRef for Nil {
//     type Target<'a> = Nil;
//
//     #[inline]
//     fn to_ref(&self) -> Self::Target<'_> {
//         Nil
//     }
// }
//
// /// A conversion from an heterogenous list of values into an heterogenous list of mutable
// /// references.
// pub trait HListMut {
//     /// The heterogenous list of mutable references.
//     type Target<'a>
//     where
//         Self: 'a;
//
//     // This is ignored because `HListMut` must be implemented for mutable references only,
//     // therefore `to_mut` must take `self`. The reason behind this design is because we don't have
//     // GATs on stable in order to write something like this:
//     // ```
//     // trait HList {
//     //     type ToRef<'a> where Self: 'a;
//     //     type ToMut<'a> where Self: 'a;
//     //
//     //     fn to_ref(&self) -> Self::ToRef<'_>;
//     //     fn to_mut(&mut self) -> Self::ToMut<'_>;
//     // }
//     // ```
//     /// Create a heterogeneous list of mutable references
//     fn to_mut(&mut self) -> Self::Target<'_>;
// }
//
// impl<T, U> HListMut for Cons<T, U>
// where
//     U: HListMut,
// {
//     type Target<'a> = Cons<&'a mut T, <U as HListMut>::Target<'a>>
//     where
//         Self: 'a;
//
//     #[inline]
//     fn to_mut(&mut self) -> Self::Target<'_> {
//         let Cons { head, tail } = self;
//         Cons {
//             head,
//             tail: tail.to_mut(),
//         }
//     }
// }
//
// impl HListMut for Nil {
//     type Target<'a> = Nil
//     where
//         Self: 'a;
//
//     #[inline]
//     fn to_mut(&mut self) -> Self::Target<'_> {
//         Nil
//     }
// }

pub trait HList {
    type Ref<'a>: HList
    where
        Self: 'a;

    type MutRef<'a>: HList
    where
        Self: 'a;

    type Reversed: HList;

    fn to_ref(&self) -> Self::Ref<'_>;
    fn to_mut(&mut self) -> Self::MutRef<'_>;

    /// Create a heterogeneous list with the elements in reverse order
    fn reverse(self) -> Self::Reversed;
}

/// An interface for non-empty heterogenous lists.
pub trait NonEmptyHList: HList {
    type Head;
    type Last;
    type Init: HList;
    type Tail: HList;

    /// Split the last element of an heterogeneous list
    ///
    /// Return a tuple with the last element and a list containing the remainder.
    fn split_last(self) -> (Self::Last, Self::Init);
}

impl HList for Nil {
    type Ref<'a> = Nil
    where
        Self: 'a;

    type MutRef<'a> = Nil
    where
        Self: 'a;

    type Reversed = Nil;

    #[inline]
    fn to_ref(&self) -> Self::Ref<'_> {
        Nil
    }

    #[inline]
    fn to_mut(&mut self) -> Self::MutRef<'_> {
        Nil
    }

    #[inline]
    fn reverse(self) -> Self::Reversed {
        Nil
    }
}

impl<T> HList for Cons<T, Nil> {
    type Ref<'a> = Cons<&'a T, Nil>
    where
        Self: 'a;

    type MutRef<'a> = Cons<&'a mut T, Nil>
    where
        Self: 'a;

    type Reversed = Self;

    #[inline]
    fn to_ref(&self) -> Self::Ref<'_> {
        Cons {
            head: &self.head,
            tail: Nil,
        }
    }

    #[inline]
    fn to_mut(&mut self) -> Self::MutRef<'_> {
        Cons {
            head: &mut self.head,
            tail: Nil,
        }
    }

    #[inline]
    fn reverse(self) -> Self::Reversed {
        self
    }
}

impl<T, U, V> HList for Cons<T, Cons<U, V>>
where
    Cons<U, V>: NonEmptyHList,
{
    type Ref<'a> = Cons<&'a T, <Cons<U, V> as HList>::Ref<'a>>
    where
        Self: 'a;

    type MutRef<'a> = Cons<&'a mut T, <Cons<U, V> as HList>::MutRef<'a>>
    where
        Self: 'a;

    type Reversed = Cons<
        <Cons<U, V> as NonEmptyHList>::Last,
        <Cons<T, <Cons<U, V> as NonEmptyHList>::Init> as HList>::Reversed,
    >;

    #[inline]
    fn to_ref(&self) -> Self::Ref<'_> {
        let Cons { head, tail } = self;

        Cons {
            head,
            tail: tail.to_ref(),
        }
    }

    #[inline]
    fn to_mut(&mut self) -> Self::MutRef<'_> {
        let Cons { head, tail } = self;

        Cons {
            head,
            tail: tail.to_mut(),
        }
    }

    #[inline]
    fn reverse(self) -> Self::Reversed {
        let Self { head, tail } = self;

        let (last, init) = tail.split_last();
        let tail = Cons { head, tail: init }.reverse();
        Cons { head: last, tail }
    }
}

impl<T> NonEmptyHList for Cons<T, Nil> {
    type Head = T;
    type Last = T;
    type Init = Nil;
    type Tail = Nil;

    #[inline]
    fn split_last(self) -> (Self::Last, Self::Init) {
        (self.head, Nil)
    }
}

impl<T, U, V> NonEmptyHList for Cons<T, Cons<U, V>>
where
    Cons<U, V>: NonEmptyHList,
{
    type Head = T;
    type Last = <Cons<U, V> as NonEmptyHList>::Last;
    type Init = Cons<T, <Cons<U, V> as NonEmptyHList>::Init>;
    type Tail = Cons<U, V>;

    #[inline]
    fn split_last(self) -> (Self::Last, Self::Init) {
        let Self { head, tail } = self;
        let (last, init) = tail.split_last();
        let init = Cons { head, tail: init };

        (last, init)
    }
}

// impl<T, U> NonEmptyHList for Cons<T, U>
// where
//     U: NonEmptyHList,
//     Cons<T, U::Init>: NonEmptyHList,
// {
//     type Init = Cons<T, U::Init>;
//     type Last = U::Last;
//     type Reversed = Cons<Self::Last, <Self::Init as NonEmptyHList>::Reversed>;

//     #[inline]
//     fn split_last(self) -> (Self::Last, Self::Init) {
//         let Self { head, tail } = self;
//         let (last, tail) = tail.split_last();
//         let init = Cons { head, tail };
//         (last, init)
//     }

//     #[inline]
//     fn reverse(self) -> Self::Reversed {
//         let (last, init) = self.split_last();
//         let tail = init.reverse();
//         Cons { head: last, tail }
//     }
// }

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

        let list = Nil::cons(A(42)).cons(B(1.234)).cons(C("C".to_string()));

        assert_eq!(
            list.split_head(),
            (C("C".to_string()), Nil::cons(A(42)).cons(B(1.234))),
        )
    }

    #[test]
    fn chain() {
        let list = Nil::cons("A").cons(2).cons("C".to_string());

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
            b: Nil::cons(B { foo: 42 }).cons(C { bar: "42" }),
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

        let list = Nil::cons(A(42)).cons(B(1.234)).cons(C("hello".to_string()));

        assert_eq!(
            list.to_ref(),
            Nil::cons(&A(42))
                .cons(&B(1.234))
                .cons(&C("hello".to_string())),
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

        let mut list = Nil::cons(A(42)).cons(B(1.234)).cons(C("hello".to_string()));

        assert_eq!(
            list.to_mut(),
            Nil::cons(&mut A(42))
                .cons(&mut B(1.234))
                .cons(&mut C("hello".to_string())),
        )
    }

    #[test]
    fn split_last() {
        #[derive(Debug, PartialEq)]
        struct A(i32);

        #[derive(Debug, PartialEq)]
        struct B(f32);

        #[derive(Debug, PartialEq)]
        struct C(String);

        let list = Nil::cons(A(42)).cons(B(1.234)).cons(C("hello".to_string()));

        let (last, init) = list.split_last();
        assert_eq!(last, A(42));
        assert_eq!(init, Nil::cons(B(1.234)).cons(C("hello".to_string())));

        let (last, init) = init.split_last();
        assert_eq!(last, B(1.234));
        assert_eq!(init, Nil::cons(C("hello".to_string())));

        let (last, init) = init.split_last();
        assert_eq!(last, C("hello".to_string()));
        assert_eq!(init, Nil);
    }

    #[test]
    fn reverse() {
        #[derive(Debug, PartialEq)]
        struct A(i32);

        #[derive(Debug, PartialEq)]
        struct B(f32);

        #[derive(Debug, PartialEq)]
        struct C(String);

        let list = Nil::cons(A(42)).cons(B(1.234)).cons(C("hello".to_string()));

        assert_eq!(
            list.reverse(),
            Nil::cons(C("hello".to_string())).cons(B(1.234)).cons(A(42)),
        )
    }
}
