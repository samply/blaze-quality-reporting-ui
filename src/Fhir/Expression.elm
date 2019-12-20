module Fhir.Expression exposing (Expression, cql, decoder, encode)

import Fhir.Encode exposing (object, optionalPair, pair)
import Fhir.PrimitiveTypes exposing (Code)
import Json.Decode exposing (Decoder, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)


type alias Expression =
    { language : Code
    , expression : Maybe String
    }


cql : Maybe String -> Expression
cql expression =
    { language = "text/cql"
    , expression = expression
    }


encode : Expression -> Value
encode { language, expression } =
    object
        [ pair "language" Encode.string language
        , optionalPair "expression" Encode.string expression
        ]


decoder : Decoder Expression
decoder =
    succeed Expression
        |> required "language" string
        |> optional "expression" (maybe string) Nothing
