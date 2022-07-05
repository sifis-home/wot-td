use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::hlist::{Cons, HList, Nil};

pub trait Extendable: Serialize + for<'a> Deserialize<'a> {}

impl<T> Extendable for T where T: Serialize + for<'a> Deserialize<'a> {}

pub trait ExtendableThing {
    type Item: Extendable;
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

pub trait ForwardExtendableThing: ExtendableThing {
    type ExtendableForm: ExtendableForm<
        Item = Self::Form,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendableInteractionAffordance: ExtendableInteractionAffordance<
        Item = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::ArraySchema,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendablePropertyAffordance: ExtendableAffordance<
        Item = Self::PropertyAffordance,
        InteractionAffordance = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::ArraySchema,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendableActionAffordance: ExtendableAffordance<
        Item = Self::ActionAffordance,
        InteractionAffordance = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::ArraySchema,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendableEventAffordance: ExtendableAffordance<
        Item = Self::EventAffordance,
        InteractionAffordance = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::ArraySchema,
        ExpectedResponse = Self::ExpectedResponse,
    >;

    type ExtendableDataSchema: ExtendableDataSchema<
        Item = Self::DataSchema,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::ArraySchema,
    >;
}

pub trait ExtendableForm {
    type Item: Extendable;
    type ExpectedResponse: Extendable;
}

pub trait ExtendableInteractionAffordance {
    type Item: Extendable;
    type DataSchema: Extendable;
    type ObjectSchema: Extendable;
    type ArraySchema: Extendable;
    type Form: Extendable;
    type ExpectedResponse: Extendable;

    type ExtendableDataSchema: ExtendableDataSchema<
        Item = Self::DataSchema,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::ArraySchema,
    >;
    type ExtendableForm: ExtendableForm<
        Item = Self::DataSchema,
        ExpectedResponse = Self::ExpectedResponse,
    >;
}

pub trait ExtendableAffordance {
    type Item: Extendable;
    type InteractionAffordance: Extendable;
    type DataSchema: Extendable;
    type ObjectSchema: Extendable;
    type ArraySchema: Extendable;
    type Form: Extendable;
    type ExpectedResponse: Extendable;

    type ExtendableInteractionAffordance: ExtendableInteractionAffordance<
        Item = Self::InteractionAffordance,
        DataSchema = Self::DataSchema,
        Form = Self::Form,
        ExpectedResponse = Self::ExpectedResponse,
    >;
    type ExtendableDataSchema: ExtendableDataSchema<
        Item = Self::DataSchema,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::ArraySchema,
    >;
}

pub trait ExtendableDataSchema {
    type Item: Extendable;
    type ObjectSchema: Extendable;
    type ArraySchema: Extendable;

    type ExtendableObjectSchema: ExtendableObjectSchema<
        Item = Self::ObjectSchema,
        DataSchemaItem = Self::Item,
        DataSchema = Self,
        ArraySchema = Self::ArraySchema,
    >;

    type ExtendableArraySchema: ExtendableArraySchema<
        Item = Self::ArraySchema,
        DataSchemaItem = Self::Item,
        DataSchema = Self,
        ObjectSchema = Self::ObjectSchema,
    >;
}

pub trait ExtendableObjectSchema {
    type Item: Extendable;
    type DataSchemaItem: Extendable;
    type ArraySchema: Extendable;

    type DataSchema: ExtendableDataSchema<
        Item = Self::DataSchemaItem,
        ObjectSchema = Self::Item,
        ArraySchema = Self::ArraySchema,
        ExtendableObjectSchema = Self,
    >;
}

pub trait ExtendableArraySchema {
    type Item: Extendable;
    type DataSchemaItem: Extendable;
    type ObjectSchema: Extendable;

    type DataSchema: ExtendableDataSchema<
        Item = Self::DataSchemaItem,
        ObjectSchema = Self::ObjectSchema,
        ArraySchema = Self::Item,
        ExtendableArraySchema = Self,
    >;
}

impl ExtendableThing for Nil {
    type Item = Nil;
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

impl ForwardExtendableThing for Nil {
    type ExtendableForm = Nil;
    type ExtendableInteractionAffordance = Nil;
    type ExtendablePropertyAffordance = Nil;
    type ExtendableActionAffordance = Nil;
    type ExtendableEventAffordance = Nil;
    type ExtendableDataSchema = Nil;
}

impl ExtendableForm for Nil {
    type Item = Nil;
    type ExpectedResponse = Nil;
}

impl ExtendableInteractionAffordance for Nil {
    type Item = Nil;
    type DataSchema = Nil;
    type ObjectSchema = Nil;
    type ArraySchema = Nil;
    type Form = Nil;
    type ExpectedResponse = Nil;
    type ExtendableDataSchema = Nil;
    type ExtendableForm = Nil;
}

impl ExtendableAffordance for Nil {
    type Item = Nil;
    type InteractionAffordance = Nil;
    type DataSchema = Nil;
    type ObjectSchema = Nil;
    type ArraySchema = Nil;
    type Form = Nil;
    type ExpectedResponse = Nil;
    type ExtendableInteractionAffordance = Nil;
    type ExtendableDataSchema = Nil;
}

impl ExtendableDataSchema for Nil {
    type Item = Nil;
    type ObjectSchema = Nil;
    type ArraySchema = Nil;

    type ExtendableObjectSchema = Nil;
    type ExtendableArraySchema = Nil;
}

impl ExtendableObjectSchema for Nil {
    type Item = Nil;
    type DataSchemaItem = Nil;
    type ArraySchema = Nil;
    type DataSchema = Nil;
}

impl ExtendableArraySchema for Nil {
    type Item = Nil;
    type DataSchemaItem = Nil;
    type ObjectSchema = Nil;
    type DataSchema = Nil;
}

// impl<T, U> ExtendableThing for Cons<T, U>
// where
//     T: ExtendableThing,
//     U: ExtendableThing,
// {
//     type Item = Cons<T::Item, U::Item>;
//     type InteractionAffordance = Cons<T::InteractionAffordance, U::InteractionAffordance>;
//     type PropertyAffordance = Cons<T::PropertyAffordance, U::PropertyAffordance>;
//     type Form = Cons<T::Form, U::Form>;
//     type ExpectedResponse = Cons<T::ExpectedResponse, U::ExpectedResponse>;
//     type DataSchema = Cons<T::DataSchema, U::DataSchema>;
// }

// impl<T, U, Form, ExpectedResponse> ForwardExtendableThing for Cons<T, U>
// where
//     T: ExtendableThing,
//     U: ForwardExtendableThing<ExtendableForm = (Form, ExpectedResponse)>,
// {
//     type ExtendableForm = (
//         Cons<T::Form, Form>,
//         Cons<T::ExpectedResponse, ExpectedResponse>,
//     );

//     type ExtendableInteractionAffordance;

//     type ExtendablePropertyAffordance;

//     type ExtendableDataSchema;
// }
