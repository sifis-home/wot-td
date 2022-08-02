use crate::extend::ExtendableThing;
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, skip_serializing_none, OneOrMany};

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub enum ControlPacket {
    #[serde(rename = "mqv:publish")]
    Publish,
    #[serde(rename = "mqv:subscribe")]
    Subscribe,
    #[serde(rename = "mqv:unsubscribe")]
    Unsubscribe,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub enum QoS {
    #[serde(rename = "quality:0")]
    AtMostOnce,
    #[serde(rename = "quality:1")]
    AtLeastOnce,
    #[serde(rename = "quality:2")]
    ExactlyOnce,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash, Default)]
pub struct Form {
    #[serde(
        rename = "mqv:retain",
        skip_serializing_if = "std::ops::Not::not",
        default
    )]
    pub retain: bool,
    #[serde(rename = "mqv:controlPacket")]
    pub control_packet: Option<ControlPacket>,
    #[serde(rename = "mqv:qos")]
    pub qos: Option<QoS>,
    #[serde(rename = "mqv:topic")]
    pub topic: Option<String>,
    #[serde(rename = "mqv:filter", default)]
    #[serde_as(as = "OneOrMany<_>")]
    pub filter: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Default)]
pub struct MqttProtocol {}

impl ExtendableThing for MqttProtocol {
    type InteractionAffordance = ();
    type PropertyAffordance = ();
    type ActionAffordance = ();
    type EventAffordance = ();
    type Form = Form;
    type ExpectedResponse = ();
    type DataSchema = ();
    type ObjectSchema = ();
    type ArraySchema = ();
}

#[cfg(test)]
mod test {
    use super::MqttProtocol;
    use crate::thing::Form;
    use crate::thing::{DefaultedFormOperations::Custom, FormOperation::*};

    fn deserialize_form(s: &str, r: Form<MqttProtocol>) {
        let f: crate::thing::Form<MqttProtocol> = serde_json::from_str(s).unwrap();
        assert_eq!(f, r);
    }

    #[test]
    fn simple_filter() {
        let s = r#"{
            "href": "mqtt://broker.com:1883",
            "op": [
                "subscribeevent",
                "unsubscribeevent"
            ],
            "mqv:filter": "thing1/events/overheating"
        }"#;

        let expected = Form {
            op: Custom(vec![SubscribeEvent, UnsubscribeEvent]),
            href: "mqtt://broker.com:1883".into(),
            other: super::Form {
                filter: vec!["thing1/events/overheating".into()],
                ..Default::default()
            },
            ..Default::default()
        };

        deserialize_form(s, expected);
    }

    #[test]
    fn complex_filter() {
        let s = r#"{
            "href": "mqtt://broker.com:1883",
            "op": [
                "readproperty",
                "observeproperty"
            ],
            "mqv:qos": "quality:1",
            "mqv:retain" : true,
            "mqv:filter": "application/devices/thing1/properties/test"
        }"#;

        let expected = Form {
            op: Custom(vec![ReadProperty, ObserveProperty]),
            href: "mqtt://broker.com:1883".into(),
            other: super::Form {
                qos: Some(super::QoS::AtLeastOnce),
                retain: true,
                filter: vec!["application/devices/thing1/properties/test".into()],
                ..Default::default()
            },
            ..Default::default()
        };

        deserialize_form(s, expected);
    }
}
