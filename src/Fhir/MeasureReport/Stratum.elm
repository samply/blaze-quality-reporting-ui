module Fhir.MeasureReport.Stratum exposing (Component, componentDecoder)

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)


type alias Component =
    { code : CodeableConcept
    , value : CodeableConcept
    }


componentDecoder : Decoder Component
componentDecoder =
    succeed Component
        |> required "code" CodeableConcept.decoder
        |> required "value" CodeableConcept.decoder
