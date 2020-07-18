module Fhir.Bundle exposing
    ( Bundle
    , decoder
    , encode
    , linkUrl
    )

import Fhir.Bundle.Entry as Entry exposing (BundleEntry)
import Fhir.Encode exposing (object, optionalListPair, pair)
import Fhir.PrimitiveTypes exposing (Uri)
import Json.Decode exposing (Decoder, int, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import List.Extra


type alias Bundle =
    { type_ : String
    , total : Maybe Int
    , link : List Link
    , entry : List BundleEntry
    }


encode : Bundle -> Value
encode { type_, link, entry } =
    object
        [ pair "resourceType" Encode.string "Bundle"
        , pair "type" Encode.string type_
        , optionalListPair "Link" encodeLink link
        , optionalListPair "entry" Entry.encode entry
        ]


decoder : Decoder Bundle
decoder =
    succeed Bundle
        |> required "type" string
        |> optional "total" (maybe int) Nothing
        |> optional "link" (list linkDecoder) []
        |> optional "entry" (list Entry.decoder) []


linkUrl : String -> Bundle -> Maybe Uri
linkUrl relation { link } =
    link
        |> List.Extra.find (.relation >> (==) relation)
        |> Maybe.map .url


type alias Link =
    { relation : String
    , url : Uri
    }


encodeLink : Link -> Value
encodeLink { relation, url } =
    object
        [ pair "relation" Encode.string relation
        , pair "url" Encode.string url
        ]


linkDecoder : Decoder Link
linkDecoder =
    succeed Link
        |> required "relation" string
        |> required "url" string
