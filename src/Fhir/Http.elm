module Fhir.Http exposing (..)

import Fhir.Bundle as Bundle exposing (Bundle)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Url.Builder as UrlBuilder exposing (QueryParameter)


searchType :
    (Result Http.Error Bundle -> msg)
    -> String
    -> String
    -> List QueryParameter
    -> Cmd msg
searchType toMsg base type_ params =
    Http.get
        { url = UrlBuilder.crossOrigin base [ type_ ] params
        , expect = Http.expectJson toMsg Bundle.decoder
        }


postBundle : (Result Http.Error Bundle -> msg) -> String -> Value -> Cmd msg
postBundle toMsg base bundle =
    Http.post
        { url = base
        , body = Http.jsonBody bundle
        , expect = Http.expectJson toMsg Bundle.decoder
        }


create :
    (Result Http.Error resource -> msg)
    -> String
    -> String
    -> Decoder resource
    -> Value
    -> Cmd msg
create toMsg base type_ decoder resource =
    Http.post
        { url = UrlBuilder.crossOrigin base [ type_ ] []
        , body = Http.jsonBody resource
        , expect = Http.expectJson toMsg decoder
        }


update :
    (Result Http.Error resource -> msg)
    -> String
    -> String
    -> String
    -> Decoder resource
    -> Value
    -> Cmd msg
update toMsg base type_ id decoder resource =
    Http.request
        { method = "PUT"
        , headers = []
        , url = UrlBuilder.crossOrigin base [ type_, id ] []
        , body = Http.jsonBody resource
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }
