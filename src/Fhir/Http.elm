module Fhir.Http exposing
    ( Error(..)
    , create
    , delete
    , postBundle
    , postOperationInstance
    , postOperationType
    , read
    , searchType
    , update
    )

{-| FHIR RESTful API interactions.


# Creating a Resource

    create CompletedCreate base "Patient" patientDecoder patient

-}

import Fhir.Bundle as Bundle exposing (Bundle)
import Fhir.OperationOutcome as OperationOutcome exposing (OperationOutcome)
import Fhir.PrimitiveTypes exposing (Id)
import Http
import Json.Decode as Decoder exposing (Decoder)
import Json.Encode exposing (Value)
import Url.Builder as UrlBuilder exposing (QueryParameter)


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int OperationOutcome
    | BadBody String


read :
    (Result Error resource -> msg)
    -> String
    -> String
    -> Id
    -> Decoder resource
    -> Cmd msg
read toMsg base type_ id decoder =
    Http.get
        { url = UrlBuilder.crossOrigin base [ type_, id ] []
        , expect = expectJson toMsg decoder
        }


searchType :
    (Result Error Bundle -> msg)
    -> String
    -> String
    -> List QueryParameter
    -> Cmd msg
searchType toMsg base type_ params =
    Http.get
        { url = UrlBuilder.crossOrigin base [ type_ ] params
        , expect = expectJson toMsg Bundle.decoder
        }


postBundle : (Result Error Bundle -> msg) -> String -> Value -> Cmd msg
postBundle toMsg base bundle =
    Http.post
        { url = base
        , body = Http.jsonBody bundle
        , expect = expectJson toMsg Bundle.decoder
        }


{-| Creates a resource.
-}
create :
    (Result Error resource -> msg)
    -> String
    -> String
    -> Decoder resource
    -> Value
    -> Cmd msg
create toMsg base type_ decoder resource =
    Http.post
        { url = UrlBuilder.crossOrigin base [ type_ ] []
        , body = Http.jsonBody resource
        , expect = expectJson toMsg decoder
        }


update :
    (Result Error resource -> msg)
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
        , expect = expectJson toMsg decoder
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


postOperationInstance :
    (Result Error a -> msg)
    -> String
    -> String
    -> Id
    -> String
    -> List QueryParameter
    -> Decoder a
    -> Cmd msg
postOperationInstance toMsg base type_ id name params decoder =
    Http.post
        { url =
            UrlBuilder.crossOrigin base
                [ type_, id, name ]
                params
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


postOperationType :
    (Result Error a -> msg)
    -> String
    -> String
    -> String
    -> List QueryParameter
    -> Decoder a
    -> Cmd msg
postOperationType toMsg base type_ name params decoder =
    Http.post
        { url = UrlBuilder.crossOrigin base [ type_, name ] []
        , body = formBody params
        , expect = expectJson toMsg decoder
        }


formBody : List QueryParameter -> Http.Body
formBody =
    UrlBuilder.toQuery
        -- drop the question mark
        >> String.dropLeft 1
        >> Http.stringBody "application/x-www-form-urlencoded"


expectJson : (Result Error a -> msg) -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    case Decoder.decodeString OperationOutcome.decoder body of
                        Ok operationOutcome ->
                            Err (BadStatus metadata.statusCode operationOutcome)

                        Err err ->
                            Err (BadBody (Decoder.errorToString err))

                Http.GoodStatus_ _ body ->
                    case Decoder.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (Decoder.errorToString err))
