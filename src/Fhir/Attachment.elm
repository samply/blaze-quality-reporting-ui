module Fhir.Attachment exposing (Attachment, decoder, encode)

import Fhir.Encode exposing (object, optionalPair)
import Fhir.PrimitiveTypes
    exposing
        ( Base64Binary
        , Code
        , base64BinaryDecoder
        , encodeBase64Binary
        )
import Json.Decode exposing (Decoder, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias Attachment =
    { contentType : Maybe Code
    , data : Maybe Base64Binary
    }


encode : Attachment -> Value
encode { contentType, data } =
    object
        [ optionalPair "contentType" Encode.string contentType
        , optionalPair "data" encodeBase64Binary data
        ]


decoder : Decoder Attachment
decoder =
    succeed Attachment
        |> optional "contentType" (maybe string) Nothing
        |> optional "data" (maybe base64BinaryDecoder) Nothing
