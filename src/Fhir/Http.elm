module Fhir.Http exposing (..)

{-| FHIR RESTful API interactions.


# Creating a Resource

    create CompletedCreate base "Patient" patientDecoder patient

-}

import Fhir.Bundle as Bundle exposing (Bundle)
import Fhir.PrimitiveTypes exposing (Id)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Url.Builder as UrlBuilder exposing (QueryParameter)


read :
    (Result Http.Error resource -> msg)
    -> String
    -> String
    -> Id
    -> Decoder resource
    -> Cmd msg
read toMsg base type_ id decoder =
    Http.get
        { url = UrlBuilder.crossOrigin base [ type_, id ] []
        , expect = Http.expectJson toMsg decoder
        }


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


{-| Creates a resource.
-}
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


delete :
    (Result Http.Error () -> msg)
    -> String
    -> String
    -> String
    -> Cmd msg
delete toMsg base type_ id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = UrlBuilder.crossOrigin base [ type_, id ] []
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
