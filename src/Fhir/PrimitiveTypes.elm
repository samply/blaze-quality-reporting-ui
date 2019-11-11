module Fhir.PrimitiveTypes exposing
    ( Base64Binary
    , Canonical
    , Code
    , Id
    , Uri
    , base64BinaryDecoder
    , encodeBase64Binary
    )

import Base64
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Uri =
    String


type alias Canonical =
    String


type alias Base64Binary =
    String


type alias Code =
    String


type alias Id =
    String


encodeBase64Binary : Base64Binary -> Value
encodeBase64Binary b =
    Base64.encode b |> Encode.string


base64BinaryDecoder : Decoder Base64Binary
base64BinaryDecoder =
    Decode.andThen
        (\s ->
            case Base64.decode s of
                Ok s_ ->
                    Decode.succeed s_

                Err e ->
                    Decode.fail e
        )
        Decode.string
