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
    /// The extension type for [`InteractionAffordance`].
    ///
    /// [`InteractionAffordance`]: crate::thing::InteractionAffordance
    type InteractionAffordance: ExtendablePiece;

    /// The extension type for [`PropertyAffordance`].
    ///
    /// [`PropertyAffordance`]: crate::thing::PropertyAffordance
    type PropertyAffordance: ExtendablePiece;

    /// The extension type for [`ActionAffordance`].
    ///
    /// [`ActionAffordance`]: crate::thing::ActionAffordance
    type ActionAffordance: ExtendablePiece;

    /// The extension type for [`EventAffordance`].
    ///
    /// [`EventAffordance`]: crate::thing::EventAffordance
    type EventAffordance: ExtendablePiece;

    /// The extension type for [`Form`].
    ///
    /// [`Form`]: crate::thing::Form
    type Form: ExtendablePiece;

    /// The extension type for [`ExpectedResponse`].
    ///
    /// [`ExpectedResponse`]: crate::thing::ExpectedResponse
    type ExpectedResponse: ExtendablePiece;

    /// The extension type for [`DataSchema`].
    ///
    /// [`DataSchema`]: crate::thing::DataSchema
    type DataSchema: ExtendablePiece;

    /// The extension type for [`ObjectSchema`].
    ///
    /// [`ObjectSchema`]: crate::thing::ObjectSchema
    type ObjectSchema: ExtendablePiece;

    /// The extension type for [`ArraySchema`].
    ///
    /// [`ArraySchema`]: crate::thing::ArraySchema
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

/// A trait representing an object that can be created empty in order to extend a `Thing`.
///
/// This is separated from the [`Extend`] trait because it is not generic and it only contains an
/// associate type.
pub trait Extendable {
    /// The empty extension type.
    type Empty;

    /// Create an empty extension
    fn empty() -> Self::Empty;
}

/// A generic trait to express an object to extend a `Thing`.
///
/// This trait represents an object that can be _extended_ with other typed expressions. It is used
/// extensively for all the _extendable_ types of a `Thing`.
///
/// The generic type `T` is the type that can be _added_ to the implemented `struct`/`enum`.
///
/// The trait is generally used in combination with the [`Extendable`] trait.
pub trait Extend<T>: Sized {
    /// The new type obtained when extending `Self`.
    type Target;

    /// Extend the current extension with an additional element
    fn ext(self, t: T) -> Self::Target;

    /// Extends the current type, passing a closure that returns `T`.
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
