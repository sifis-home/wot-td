//! Structured extensions for building and parsing Thing Descriptions
//!
//! The Thing Description can be extended with additional ontologies using its JSON-LD [@context](https://www.w3.org/TR/json-ld11/#the-context).
//!
//! This module provides a trait, [ExtendableThing], to define extensions for each of the standard
//! elements of a description.

use serde::{Deserialize, Serialize};

use crate::hlist::{Cons, Nil};

/// Requirement trait for extending a Thing Description element
pub trait ExtendablePiece: Serialize + for<'a> Deserialize<'a> {}

impl<T> ExtendablePiece for T where T: Serialize + for<'a> Deserialize<'a> {}

/// Main extension trait
///
/// The trait uses an associated type for each element of the ThingDescription, set it to `()` if
/// the extension does not apply to that specific element.
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

    /// Create an empty extension
    fn empty() -> Self::Empty;
}

pub trait Extend<T>: Sized {
    type Target;

    /// Extend the current extension with an additional element
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
        Nil::cons(t)
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
        self.cons(t)
    }
}
