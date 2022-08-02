//! CoAP Binding Template

use crate::extend::ExtendableThing;
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use serde_with::{serde_as, skip_serializing_none};

/// CoAP request method
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Method {
    Get,
    Put,
    Post,
    Delete,
    Patch,
    Fetch,
    #[serde(rename = "iPATCH")]
    Ipatch,
}

/// CoAP Allowed block size
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize_repr, Serialize_repr)]
#[repr(u16)]
pub enum BlockSize {
    Szx16 = 16,
    Szx32 = 32,
    Szx64 = 64,
    Szx128 = 128,
    Szx256 = 256,
    Szx512 = 512,
    Szx1024 = 1024,
}

/// CoAP BlockWise Transfer Parameters
///
/// They may apply to Block-Wise Transfers [RFC7959] or
/// Block-Wise Transfer Options Supporting Robust Transmission [RFC9177].
///
/// [RFC7959]: https://www.rfc-editor.org/rfc/rfc7959.html
/// [RFC9177]: https://www.rfc-editor.org/rfc/rfc9177.html
#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, Hash, Default)]
pub struct BlockWiseTransferParameters {
    #[serde(rename = "cov:block2SZX")]
    pub block_2szx: Option<BlockSize>,
    #[serde(rename = "cov:block1SZX")]
    pub block_1szx: Option<BlockSize>,
}

/// CoAP Protocol Form fields
#[serde_as]
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash, Default)]
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

/// Extension for the CoAP protocol
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash, Default)]
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
    use super::{BlockSize, CoapProtocol};
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
                    block_2szx: Some(BlockSize::Szx64),
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
