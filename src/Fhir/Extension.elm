module Fhir.Extension exposing (Extension, Value(..), decoder, encode)

import Fhir.Encode exposing (object, optionalPair, pair)
import Fhir.PrimitiveTypes exposing (Uri)
import Json.Decode exposing (Decoder, field, int, map, maybe, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode


type alias Extension =
    { url : Uri
    , value : Maybe Value
    }


type Value
    = Integer Int


integerValue : Extension -> Maybe Int
integerValue { value } =
    case value of
        Just (Integer i) ->
            Just i

        _ ->
            Nothing


encode : Extension -> Encode.Value
encode ({ url } as expression) =
    object
        [ pair "url" Encode.string url
        , optionalPair "valueInteger" Encode.int (integerValue expression)
        ]


decoder : Decoder Extension
decoder =
    succeed Extension
        |> required "url" string
        |> custom valueDecoder


valueDecoder : Decoder (Maybe Value)
valueDecoder =
    oneOf
        [ map Integer (field "valueInteger" int)
        ]
        |> maybe
