{
   "@context" : [
      "https://www.w3.org/2019/wot/td/v1",
      "https://www.w3.org/2022/wot/td/v1.1",
      {
         "iot" : "http://example.org/iot"
      },
      {
         "@language" : "en"
      }
   ],
   "@type" : "Thing",
   "actions" : {
      "decrement" : {
         "description" : "Decrementing counter value",
         "descriptions" : {
            "de" : "Zähler verringern",
            "en" : "Decrementing counter value",
            "it" : "decrementare valore"
         },
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/actions/decrement",
               "htv:methodName" : "POST",
               "op" : [
                  "invokeaction"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/actions/decrement",
               "htv:methodName" : "POST",
               "op" : [
                  "invokeaction"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/actions/decrement",
               "op" : "invokeaction"
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/actions/decrement",
               "op" : "invokeaction"
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/actions/decrement",
               "op" : [
                  "invokeaction"
               ]
            }
         ],
         "idempotent" : false,
         "safe" : false
      },
      "increment" : {
         "description" : "Incrementing counter value",
         "descriptions" : {
            "de" : "Zähler erhöhen",
            "en" : "Incrementing counter value",
            "it" : "incrementare valore"
         },
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/actions/increment",
               "htv:methodName" : "POST",
               "op" : [
                  "invokeaction"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/actions/increment",
               "htv:methodName" : "POST",
               "op" : [
                  "invokeaction"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/actions/increment",
               "op" : "invokeaction"
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/actions/increment",
               "op" : "invokeaction"
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/actions/increment",
               "op" : [
                  "invokeaction"
               ]
            }
         ],
         "idempotent" : false,
         "safe" : false
      },
      "reset" : {
         "description" : "Resetting counter value",
         "descriptions" : {
            "de" : "Zähler resettieren",
            "en" : "Resetting counter value",
            "it" : "resettare valore"
         },
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/actions/reset",
               "htv:methodName" : "POST",
               "op" : [
                  "invokeaction"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/actions/reset",
               "htv:methodName" : "POST",
               "op" : [
                  "invokeaction"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/actions/reset",
               "op" : "invokeaction"
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/actions/reset",
               "op" : "invokeaction"
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/actions/reset",
               "op" : [
                  "invokeaction"
               ]
            }
         ],
         "idempotent" : false,
         "safe" : false
      }
   },
   "description" : "counter example Thing",
   "descriptions" : {
      "de" : "Zähler Beispiel Ding",
      "en" : "counter example Thing",
      "it" : "Contatore Esempio"
   },
   "events" : {
      "change" : {
         "description" : "change event",
         "descriptions" : {
            "de" : "Änderungsnachricht",
            "en" : "change event",
            "it" : "resettare valore"
         },
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/events/change",
               "op" : [
                  "subscribeevent",
                  "unsubscribeevent"
               ],
               "subprotocol" : "longpoll"
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/events/change",
               "op" : [
                  "subscribeevent",
                  "unsubscribeevent"
               ],
               "subprotocol" : "longpoll"
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/events/change",
               "op" : [
                  "subscribeevent",
                  "unsubscribeevent"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/events/change",
               "op" : [
                  "subscribeevent",
                  "unsubscribeevent"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/events/change",
               "op" : [
                  "subscribeevent",
                  "unsubscribeevent"
               ]
            }
         ]
      }
   },
   "forms" : [
      {
         "contentType" : "application/json",
         "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/properties",
         "op" : [
            "readallproperties",
            "readmultipleproperties"
         ]
      },
      {
         "contentType" : "application/json",
         "href" : "http://192.168.1.212:8080/counter/properties",
         "op" : [
            "readallproperties",
            "readmultipleproperties"
         ]
      }
   ],
   "id" : "urn:uuid:362dfaae-54b3-4de2-8776-4469fa146b27",
   "properties" : {
      "count" : {
         "description" : "current counter value",
         "descriptions" : {
            "de" : "Derzeitiger Zähler Stand",
            "en" : "current counter value",
            "it" : "valore attuale del contatore"
         },
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/properties/count",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/properties/count/observable",
               "op" : [
                  "observeproperty",
                  "unobserveproperty"
               ],
               "subprotocol" : "longpoll"
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/properties/count",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/properties/count/observable",
               "op" : [
                  "observeproperty",
                  "unobserveproperty"
               ],
               "subprotocol" : "longpoll"
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/properties/count",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/properties/count",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/properties/count",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            }
         ],
         "iot:Custom" : "example annotation",
         "observable" : true,
         "readOnly" : true,
         "type" : "integer",
         "writeOnly" : false
      },
      "countAsImage" : {
         "description" : "current counter value as SVG image",
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/properties/countAsImage{?fill}",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/properties/countAsImage{?fill}",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/properties/countAsImage",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/properties/countAsImage",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/properties/countAsImage",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            }
         ],
         "observable" : false,
         "readOnly" : true,
         "uriVariables" : {
            "fill" : {
               "type" : "string"
            }
         },
         "writeOnly" : false
      },
      "lastChange" : {
         "description" : "last change of counter value",
         "descriptions" : {
            "de" : "Letzte Änderung",
            "en" : "last change of counter value",
            "it" : "ultima modifica del valore"
         },
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/properties/lastChange",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/properties/lastChange/observable",
               "op" : [
                  "observeproperty",
                  "unobserveproperty"
               ],
               "subprotocol" : "longpoll"
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/properties/lastChange",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/properties/lastChange/observable",
               "op" : [
                  "observeproperty",
                  "unobserveproperty"
               ],
               "subprotocol" : "longpoll"
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/properties/lastChange",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/properties/lastChange",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/properties/lastChange",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            }
         ],
         "observable" : true,
         "readOnly" : true,
         "type" : "string",
         "writeOnly" : false
      },
      "redDotImage" : {
         "description" : "Red dot image as PNG",
         "forms" : [
            {
               "contentType" : "application/json",
               "href" : "http://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:8080/counter/properties/redDotImage",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "http://192.168.1.212:8080/counter/properties/redDotImage",
               "htv:methodName" : "GET",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://[fd82:b0cd:ba94:0:1cb5:873f:f3c5:9a4]:5683/counter/properties/redDotImage",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "coap://192.168.1.212:5683/counter/properties/redDotImage",
               "op" : [
                  "readproperty"
               ]
            },
            {
               "contentType" : "application/json",
               "href" : "mqtt://test.mosquitto.org/counter/properties/redDotImage",
               "op" : [
                  "readproperty",
                  "observeproperty",
                  "unobserveproperty"
               ]
            }
         ],
         "observable" : false,
         "readOnly" : true,
         "writeOnly" : false
      }
   },
   "security" : [
      "nosec_sc"
   ],
   "securityDefinitions" : {
      "nosec_sc" : {
         "scheme" : "nosec"
      }
   },
   "support" : "git://github.com/eclipse/thingweb.node-wot.git",
   "title" : "counter",
   "titles" : {
      "de" : "zähler",
      "en" : "counter",
      "it" : "Contatore"
   },
   "uriVariables" : {
      "step" : {
         "maximum" : 250,
         "minimum" : 1,
         "type" : "integer"
      }
   }
}
