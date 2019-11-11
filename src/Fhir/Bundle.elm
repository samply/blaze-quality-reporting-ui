module Fhir.Bundle exposing (Bundle, Entry, decoder, encode)

import Json.Decode exposing (Decoder, int, list, maybe, string, succeed, value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)


type alias Bundle =
    { type_ : String
    , total : Maybe Int
    , entry : List Entry
    }


type alias Entry =
    { resource : Value
    , request : Maybe Request
    }


type alias Request =
    { method : String
    , url : String
    }


encode : Bundle -> Value
encode { type_, entry } =
    Encode.object
        [ ( "resourceType", Encode.string "Bundle" )
        , ( "type", Encode.string type_ )
        , ( "entry"
          , Encode.list encodeEntry entry
          )
        ]


encodeEntry : Entry -> Value
encodeEntry { resource, request } =
    Encode.object
        ([ ( "resource", resource ) ]
            ++ (case request of
                    Just aRequest ->
                        [ ( "request", encodeRequest aRequest ) ]

                    Nothing ->
                        []
               )
        )


encodeRequest : Request -> Value
encodeRequest { method, url } =
    Encode.object
        [ ( "method", Encode.string method )
        , ( "url", Encode.string url )
        ]


decoder : Decoder Bundle
decoder =
    succeed Bundle
        |> required "type" string
        |> optional "total" (maybe int) Nothing
        |> optional "entry" (list entryDecoder) []


entryDecoder : Decoder Entry
entryDecoder =
    succeed Entry
        |> optional "resource" value Encode.null
        |> optional "request" (maybe requestDecoder) Nothing


requestDecoder : Decoder Request
requestDecoder =
    succeed Request
        |> required "method" string
        |> required "url" string
