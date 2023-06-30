//! Binding Templates Protocol Extensions
//!
//! Application layer [protocol specific templates](https://w3c.github.io/wot-binding-templates):
//! > Most protocols have a relatively small set of methods that define
//! the message type, the semantic intention of the message.
//! REST and PubSub architecture patterns result in different protocols
//! with different methods.
//! Common methods found in these protocols are GET, PUT, POST, DELETE,
//! PUBLISH, and SUBSCRIBE.
//! Binding Templates describe how these existing methods and vocabularies
//! can be described in a Thing Description.
//!

pub mod coap;
pub mod http;
pub mod mqtt;
