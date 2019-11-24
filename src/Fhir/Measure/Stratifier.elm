module Fhir.Measure.Stratifier exposing
    ( Component
    , componentDecoder
    , encodeComponent
    , newComponent
    )

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Fhir.Encode exposing (object, optionalPair, pair)
import Fhir.Expression as Expression exposing (Expression)
import Json.Decode exposing (Decoder, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)


type alias Component =
    { code : Maybe CodeableConcept
    , description : Maybe String
    , criteria : Expression
    }


newComponent : Component
newComponent =
    { code = Nothing
    , description = Nothing
    , criteria = Expression.cql Nothing
    }


encodeComponent : Component -> Value
encodeComponent { code, description, criteria } =
    object
        [ optionalPair "code" CodeableConcept.encode code
        , optionalPair "description" Encode.string description
        , pair "criteria" Expression.encode criteria
        ]


componentDecoder : Decoder Component
componentDecoder =
    succeed Component
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> optional "description" (maybe string) Nothing
        |> required "criteria" Expression.decoder
