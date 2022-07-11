use serde::{Deserialize, Serialize};

use crate::hlist::{Cons, Nil};

pub trait Extendable: Serialize + for<'a> Deserialize<'a> {}

impl<T> Extendable for T where T: Serialize + for<'a> Deserialize<'a> {}

pub trait ExtendableThing {
    type InteractionAffordance: Extendable;
    type PropertyAffordance: Extendable;
    type ActionAffordance: Extendable;
    type EventAffordance: Extendable;
    type Form: Extendable;
    type ExpectedResponse: Extendable;
    type DataSchema: Extendable;
    type ObjectSchema: Extendable;
    type ArraySchema: Extendable;
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
