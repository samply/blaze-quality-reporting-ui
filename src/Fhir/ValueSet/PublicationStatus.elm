module Fhir.ValueSet.PublicationStatus exposing
    ( PublicationStatus(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type PublicationStatus
    = Draft
    | Active
    | Retired
    | Unknown


encode : PublicationStatus -> Value
encode =
    toString >> Encode.string


toString : PublicationStatus -> String
toString status =
    case status of
        Draft ->
            "draft"

        Active ->
            "active"

        Retired ->
            "retired"

        Unknown ->
            "unknown"


decoder : Decoder PublicationStatus
decoder =
    Decode.map
        (\s ->
            case s of
                "draft" ->
                    Draft

                "active" ->
                    Active

                "retired" ->
                    Retired

                _ ->
                    Unknown
        )
        Decode.string
