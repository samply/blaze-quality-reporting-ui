module Fhir.Bundle.Entry exposing
    ( BundleEntry
    , decodeResource
    , decoder
    , encode
    , requestEntry
    , simpleRequest
    )

import Fhir.Encode exposing (object, optionalPair, pair)
import Fhir.PrimitiveTypes exposing (Uri)
import Json.Decode as Decode exposing (Decoder, decodeValue, maybe, string, succeed, value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)


type alias BundleEntry =
    { fullUrl : Maybe Uri
    , resource : Maybe Value
    , request : Maybe Request
    }


requestEntry : Request -> BundleEntry
requestEntry request =
    { fullUrl = Nothing, resource = Nothing, request = Just request }


type alias Request =
    { method : String
    , url : String
    }


simpleRequest : String -> String -> Request
simpleRequest method url =
    { method = method
    , url = url
    }


encode : BundleEntry -> Value
encode { fullUrl, resource, request } =
    object
        [ optionalPair "fullUrl" Encode.string fullUrl
        , optionalPair "resource" identity resource
        , optionalPair "request" encodeRequest request
        ]


encodeRequest : Request -> Value
encodeRequest { method, url } =
    object
        [ pair "method" Encode.string method
        , pair "url" Encode.string url
        ]


decoder : Decoder BundleEntry
decoder =
    succeed BundleEntry
        |> optional "fullUrl" (maybe string) Nothing
        |> optional "resource" (maybe value) Nothing
        |> optional "request" (maybe requestDecoder) Nothing


decodeResource : Decoder a -> BundleEntry -> Result Decode.Error a
decodeResource resourceDecoder entry =
    entry.resource
        |> Maybe.map (decodeValue resourceDecoder)
        |> Maybe.withDefault (Err (resourceDecodeError entry))


resourceDecodeError entry =
    Decode.Failure "Missing resource in bundle entry" (encode entry)


requestDecoder : Decoder Request
requestDecoder =
    succeed Request
        |> required "method" string
        |> required "url" string
