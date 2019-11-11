module Fhir.CodeableConcept exposing (CodeableConcept, decoder, encode)

import Fhir.Coding as Coding exposing (Coding)
import Json.Decode exposing (Decoder, list, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias CodeableConcept =
    { coding : List Coding }


encode : CodeableConcept -> Value
encode { coding } =
    Encode.object
        [ ( "coding", Encode.list Coding.encode coding ) ]


decoder : Decoder CodeableConcept
decoder =
    succeed CodeableConcept
        |> optional "coding" (list Coding.decoder) []
