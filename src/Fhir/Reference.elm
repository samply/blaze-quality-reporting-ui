module Fhir.Reference exposing (Reference, decoder, empty, encode)

import Fhir.Encode exposing (object, optionalPair)
import Fhir.PrimitiveTypes exposing (Uri)
import Json.Decode exposing (Decoder, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias Reference =
    { reference : Maybe String
    , type_ : Maybe Uri

    --, identifier : Maybe Identifier
    , display : Maybe String
    }


empty : Reference
empty =
    { reference = Nothing, type_ = Nothing, display = Nothing }


encode : Reference -> Value
encode { reference, type_, display } =
    object
        [ optionalPair "reference" Encode.string reference
        , optionalPair "type" Encode.string type_
        , optionalPair "display" Encode.string display
        ]


decoder : Decoder Reference
decoder =
    succeed Reference
        |> optional "reference" (maybe string) Nothing
        |> optional "type" (maybe string) Nothing
        |> optional "display" (maybe string) Nothing
