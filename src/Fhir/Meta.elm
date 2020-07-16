module Fhir.Meta exposing (Meta, decoder, encode)

import Fhir.Encode exposing (object, optionalPair)
import Fhir.PrimitiveTypes.Instant as Instant exposing (Instant)
import Json.Decode exposing (Decoder, maybe, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode exposing (Value)


type alias Meta =
    { lastUpdated : Maybe Instant }


encode : Meta -> Value
encode meta =
    object
        [ optionalPair "lastUpdated" Instant.encode meta.lastUpdated ]


decoder : Decoder Meta
decoder =
    succeed Meta
        |> optional "lastUpdated" (maybe Instant.decoder) Nothing
