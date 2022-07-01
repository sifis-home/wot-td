use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::hlist::{Cons, HList, Nil};

pub trait Extendable: Clone + Debug + PartialEq + Serialize + for<'a> Deserialize<'a> {}

impl<T> Extendable for T where T: Clone + Debug + PartialEq + Serialize + for<'a> Deserialize<'a> {}

pub trait ExtendableThing {
    type Item: Extendable;
    type InteractionAffordance: Extendable;
    type PropertyAffordance: Extendable;
    type Form: Extendable;
    type ExpectedResponse: Extendable;
    type DataSchema: Extendable;
}

pub trait ForwardExtendableThing: ExtendableThing {
    type ExtendableForm: ExtendableForm<
        Item = Self::Form,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendableInteractionAffordance: ExtendableInteractionAffordance<
        Item = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendablePropertyAffordance: ExtendablePropertyAffordance<
        Item = Self::PropertyAffordance,
        InteractionAffordance = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendableDataSchema: ExtendableDataSchema<Item = Self::DataSchema>;
}

pub trait ExtendableForm: Extendable {
    type Item: Extendable;
    type ExpectedResponse: Extendable;
}

pub trait ExtendableInteractionAffordance: Extendable {
    type Item: Extendable;
    type DataSchema: Extendable;
    type Form: Extendable;
    type ExpectedResponse: Extendable;

    type ExtendableDataSchema: ExtendableDataSchema<Item = Self::DataSchema>;
    type ExtendableForm: ExtendableForm<
        Item = Self::DataSchema,
        ExpectedResponse = Self::ExpectedResponse,
    >;
}

pub trait ExtendablePropertyAffordance: Extendable {
    type Item: Extendable;
    type InteractionAffordance: Extendable;
    type DataSchema: Extendable;
    type Form: Extendable;
    type ExpectedResponse: Extendable;

    type ExtendableInteractionAffordance: ExtendableInteractionAffordance<
        Item = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ExpectedResponse = Self::ExpectedResponse,
    >;
    type ExtendableDataSchema: ExtendableDataSchema<Item = Self::DataSchema>;
}

pub trait ExtendableDataSchema: Extendable {
    type Item: Extendable;
}

pub trait ExtendableObjectSchema: Extendable {
    type Item: Extendable;
    type DataSchema: Extendable;

    type ExtendableDataSchema: ExtendableDataSchema<Item = Self::DataSchema>;
}

impl ExtendableThing for Nil {
    type Item = Nil;
    type InteractionAffordance = Nil;
    type PropertyAffordance = Nil;
    type Form = Nil;
    type ExpectedResponse = Nil;
    type DataSchema = Nil;
}

impl ForwardExtendableThing for Nil {
    type ExtendableForm = Nil;
    type ExtendableInteractionAffordance = Nil;
    type ExtendablePropertyAffordance = Nil;
    type ExtendableDataSchema = Nil;
}

impl ExtendableForm for Nil {
    type Item = Nil;
    type ExpectedResponse = Nil;
}

impl ExtendableInteractionAffordance for Nil {
    type Item = Nil;
    type DataSchema = Nil;
    type Form = Nil;
    type ExpectedResponse = Nil;
    type ExtendableDataSchema = Nil;
    type ExtendableForm = Nil;
}

impl ExtendablePropertyAffordance for Nil {
    type Item = Nil;
    type InteractionAffordance = Nil;
    type DataSchema = Nil;
    type Form = Nil;
    type ExpectedResponse = Nil;
    type ExtendableInteractionAffordance = Nil;
    type ExtendableDataSchema = Nil;
}

impl ExtendableDataSchema for Nil {
    type Item = Nil;
}

impl ExtendableObjectSchema for Nil {
    type Item = Nil;
    type DataSchema = Nil;
    type ExtendableDataSchema = Nil;
}

impl<T, U> ExtendableThing for Cons<T, U>
where
    T: ExtendableThing,
    U: ExtendableThing,
{
    type Item = Cons<T::Item, U::Item>;
    type InteractionAffordance = Cons<T::InteractionAffordance, U::InteractionAffordance>;
    type PropertyAffordance = Cons<T::PropertyAffordance, U::PropertyAffordance>;
    type Form = Cons<T::Form, U::Form>;
    type ExpectedResponse = Cons<T::ExpectedResponse, U::ExpectedResponse>;
    type DataSchema = Cons<T::DataSchema, U::DataSchema>;
}
