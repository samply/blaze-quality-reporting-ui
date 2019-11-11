module Fhir.Coding exposing (Coding, decoder, encode)

import Fhir.PrimitiveTypes exposing (Code, Uri)
import Json.Decode exposing (Decoder, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias Coding =
    { system : Maybe Uri
    , version : Maybe String
    , code : Maybe Code
    }


encode : Coding -> Value
encode { system, version, code } =
    Encode.object <|
        List.filterMap identity
            [ Maybe.map (\s -> ( "system", Encode.string s )) system
            , Maybe.map (\s -> ( "version", Encode.string s )) version
            , Maybe.map (\s -> ( "code", Encode.string s )) code
            ]


decoder : Decoder Coding
decoder =
    succeed Coding
        |> optional "system" (maybe string) Nothing
        |> optional "version" (maybe string) Nothing
        |> optional "code" (maybe string) Nothing
