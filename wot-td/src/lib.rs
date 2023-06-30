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
//!
//! # Basic usage
//!
//! ```
//! use wot_td::{
//!     builder::*,
//!     thing::Thing,
//! };
//!
//! let thing = Thing::builder("My lamp")
//!     .finish_extend()
//!     .id("urn:dev:ops:my-lamp-1234")
//!     .attype("OnOffSwitch")
//!     .attype("Light")
//!     .description("A web connected lamp")
//!     .security(|b| b.no_sec().with_key("nosec_sc").required())
//!     .property("on", |b| {
//!         b.finish_extend_data_schema()
//!             .attype("OnOffProperty")
//!             .title("On/Off")
//!             .description("Whether the lamp is turned on")
//!             .form(|b| b.href("/properties/on"))
//!             .bool()
//!     })
//!     .property("brightness", |b| {
//!         b.finish_extend_data_schema()
//!             .attype("BrightnessProperty")
//!             .title("Brightness")
//!             .description("The level of light from 0-100")
//!             .form(|b| b.href("/properties/brightness"))
//!             .integer()
//!             .minimum(0)
//!             .maximum(100)
//!             .unit("percent")
//!     })
//!     .action("fade", |b| {
//!         b.title("Fade")
//!             .description("Fade the lamp to a given level")
//!             .form(|b| b.href("/actions/fade"))
//!             .input(|b| {
//!                 b.finish_extend()
//!                     .object()
//!                     .property("brightness", true, |b| {
//!                         b.finish_extend()
//!                             .integer()
//!                             .minimum(0)
//!                             .maximum(100)
//!                             .unit("percent")
//!                     })
//!                     .property("duration", true, |b| {
//!                         b.finish_extend().integer().minimum(1).unit("milliseconds")
//!                     })
//!             })
//!     })
//!     .event("overheated", |b| {
//!         b.description("The lamp has exceeded its safe operating temperature")
//!             .form(|b| b.href("/events/overheated"))
//!             .data(|b| b.finish_extend().number().unit("degree celsius"))
//!     });
//! #
//! # drop(thing);
//! ```
//!
//! See [`builder`] module for more examples.

pub mod builder;
pub mod extend;
pub mod hlist;
pub mod protocol;
pub mod thing;

pub use crate::thing::Thing;
