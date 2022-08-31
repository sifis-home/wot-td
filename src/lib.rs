//! Web of Things Thing Description manipulation
//!
//! Create, parse and modify [Thing Descriptions](https://www.w3.org/TR/wot-thing-description/):
//! > A Thing Description describes the metadata and interfaces of Things,
//! > where a Thing is an abstraction of a physical or virtual entity that
//! > provides interactions to and participates in the Web of Things.
//!
//! The crate relies on [serde](https://docs.rs/serde) to serialize and deserialize and provides
//! an high level [builder](builder::ThingBuilder) to ensure a valid TD is built and a set of
//! accessors to make easier to operate with complex descriptions.

pub mod builder;
pub mod extend;
pub mod hlist;
pub mod thing;

pub use thing::Thing;
