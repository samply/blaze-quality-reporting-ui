module Fhir.Expression exposing (Expression, decoder, encode)

import Fhir.PrimitiveTypes exposing (Code, Uri)
import Json.Decode exposing (Decoder, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)


type alias Expression =
    { language : Code
    , expression : Maybe String
    }


encode : Expression -> Value
encode { language, expression } =
    Encode.object <|
        List.filterMap identity
            [ Just ( "language", Encode.string language )
            , Maybe.map (\s -> ( "expression", Encode.string s )) expression
            ]


decoder : Decoder Expression
decoder =
    succeed Expression
        |> required "language" string
        |> optional "expression" (maybe string) Nothing
