use serde::{Deserialize, Serialize};
use serde_with::{serde_as, skip_serializing_none};

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Method {
    Get,
    Put,
    Post,
    Delete,
    Patch,
    #[serde(rename = "iPATCH")]
    Ipatch,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash, Default)]
pub struct BlockWiseTransferParameters {
    #[serde(rename = "cov:block2SZX")]
    pub block_2szx: Option<u16>,
    #[serde(rename = "cov:block1SZX")]
    pub block_1szx: Option<u16>,
}

#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash, Default)]
// #[serde(rename_all = "camelCase")]
pub struct Form {
    #[serde(rename = "cov:method")]
    pub method: Option<Method>,
    #[serde(rename = "cov:blockwise")]
    pub blockwise: Option<BlockWiseTransferParameters>,
    #[serde(rename = "cov:qblockwise")]
    pub qblockwise: Option<BlockWiseTransferParameters>,
    #[serde(rename = "cov:hopLimit")]
    pub hop_limit: Option<u8>,
}

use crate::extend::ExtendableThing;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Default)]
pub struct CoapProtocol {}

impl ExtendableThing for CoapProtocol {
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
    use super::CoapProtocol;
    use crate::thing::Form;
    fn deserialize_form(s: &str, r: Form<CoapProtocol>) {
        let f: crate::thing::Form<CoapProtocol> = serde_json::from_str(s).unwrap();

        assert_eq!(f, r);
    }

    #[test]
    fn deserialize_observe() {
        let form = r#"
            {
                "cov:method": "GET",
                "href": "coap://[2001:DB8::1]/status",
                "contentType": "text/plain;charset=utf-8",
                "subprotocol": "cov:observe",
                "op": ["observeproperty"]
            }
        "#;
        let expected = Form {
            op: crate::thing::DefaultedFormOperations::Custom(vec![
                crate::thing::FormOperation::ObserveProperty,
            ]),
            href: "coap://[2001:DB8::1]/status".into(),
            content_type: Some("text/plain;charset=utf-8".into()),
            subprotocol: Some("cov:observe".into()),
            other: super::Form {
                method: Some(super::Method::Get),
                ..Default::default()
            },
            ..Default::default()
        };

        deserialize_form(form, expected);
    }

    #[test]
    fn deserialize_blockwise() {
        let form = r#"
            {
                "href": "coap://[2001:DB8::1]/status",
                "contentType": "text/plain;charset=utf-8",
                "cov:blockwise": { }
            }
        "#;
        let expected = Form {
            href: "coap://[2001:DB8::1]/status".into(),
            content_type: Some("text/plain;charset=utf-8".into()),
            other: super::Form {
                blockwise: Some(super::BlockWiseTransferParameters::default()),
                ..Default::default()
            },
            ..Default::default()
        };

        deserialize_form(form, expected);
    }

    #[test]
    fn deserialize_qblockwise_params() {
        let form = r#"
            {
                "href": "coap://[2001:DB8::1]/status",
                "contentType": "text/plain;charset=utf-8",
                "cov:qblockwise": {
                    "cov:block2SZX": 64
                }
            }
        "#;
        let expected = Form {
            href: "coap://[2001:DB8::1]/status".into(),
            content_type: Some("text/plain;charset=utf-8".into()),
            other: super::Form {
                qblockwise: Some(super::BlockWiseTransferParameters {
                    block_2szx: Some(64),
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        };

        deserialize_form(form, expected);
    }

    #[test]
    fn deserialize_hop_limit() {
        let form = r#"
            {
                "href": "coap://[2001:DB8::1]/status",
                "contentType": "text/plain;charset=utf-8",
                "cov:hopLimit": 5
            }
        "#;
        let expected = Form {
            href: "coap://[2001:DB8::1]/status".into(),
            content_type: Some("text/plain;charset=utf-8".into()),
            other: super::Form {
                hop_limit: Some(5),
                ..Default::default()
            },
            ..Default::default()
        };

        deserialize_form(form, expected);
    }
}
