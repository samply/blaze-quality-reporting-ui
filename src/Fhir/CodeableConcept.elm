module Fhir.CodeableConcept exposing (CodeableConcept, decoder, encode)

import Fhir.Coding as Coding exposing (Coding)
import Fhir.Encode exposing (object, optionalListPair, optionalPair)
import Json.Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias CodeableConcept =
    { coding : List Coding
    , text : Maybe String
    }


encode : CodeableConcept -> Value
encode { coding, text } =
    object
        [ optionalListPair "coding" Coding.encode coding
        , optionalPair "text" Encode.string text
        ]


decoder : Decoder CodeableConcept
decoder =
    succeed CodeableConcept
        |> optional "coding" (list Coding.decoder) []
        |> optional "text" (maybe string) Nothing
