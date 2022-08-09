use serde::{Deserialize, Serialize};

use crate::hlist::{Cons, Nil};

pub trait ExtendablePiece: Serialize + for<'a> Deserialize<'a> {}

impl<T> ExtendablePiece for T where T: Serialize + for<'a> Deserialize<'a> {}

pub trait ExtendableThing {
    type InteractionAffordance: ExtendablePiece;
    type PropertyAffordance: ExtendablePiece;
    type ActionAffordance: ExtendablePiece;
    type EventAffordance: ExtendablePiece;
    type Form: ExtendablePiece;
    type ExpectedResponse: ExtendablePiece;
    type DataSchema: ExtendablePiece;
    type ObjectSchema: ExtendablePiece;
    type ArraySchema: ExtendablePiece;
}

impl ExtendableThing for Nil {
    type InteractionAffordance = Nil;
    type PropertyAffordance = Nil;
    type ActionAffordance = Nil;
    type EventAffordance = Nil;
    type Form = Nil;
    type ExpectedResponse = Nil;
    type DataSchema = Nil;
    type ObjectSchema = Nil;
    type ArraySchema = Nil;
}

impl<T, U> ExtendableThing for Cons<T, U>
where
    T: ExtendableThing,
    U: ExtendableThing,
{
    type InteractionAffordance = Cons<T::InteractionAffordance, U::InteractionAffordance>;
    type PropertyAffordance = Cons<T::PropertyAffordance, U::PropertyAffordance>;
    type ActionAffordance = Cons<T::ActionAffordance, U::ActionAffordance>;
    type EventAffordance = Cons<T::EventAffordance, U::EventAffordance>;
    type Form = Cons<T::Form, U::Form>;
    type ExpectedResponse = Cons<T::ExpectedResponse, U::ExpectedResponse>;
    type DataSchema = Cons<T::DataSchema, U::DataSchema>;
    type ObjectSchema = Cons<T::ObjectSchema, U::ObjectSchema>;
    type ArraySchema = Cons<T::ArraySchema, U::ArraySchema>;
}

pub trait Extendable {
    type Empty;

    fn empty() -> Self::Empty;
}

pub trait Extend<T>: Sized {
    type Target;

    fn ext(self, t: T) -> Self::Target;

    fn ext_with<F>(self, f: F) -> Self::Target
    where
        F: FnOnce() -> T,
    {
        self.ext(f())
    }
}

impl Extendable for Nil {
    type Empty = Nil;

    fn empty() -> Self {
        Nil
    }
}

impl<T> Extend<T> for Nil {
    type Target = Cons<T, Nil>;

    fn ext(self, t: T) -> Self::Target {
        Cons::new_head(t)
    }
}

impl<T, U> Extendable for Cons<T, U> {
    type Empty = Nil;

    fn empty() -> Self::Empty {
        Nil
    }
}

impl<T, U, V> Extend<T> for Cons<U, V> {
    type Target = Cons<T, Cons<U, V>>;

    fn ext(self, t: T) -> Self::Target {
        self.add(t)
    }
}
