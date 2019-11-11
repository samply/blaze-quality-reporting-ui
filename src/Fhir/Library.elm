module Fhir.Library exposing (Library, decoder, encode, new)

import Fhir.Attachment as Attachment exposing (Attachment)
import Fhir.PrimitiveTypes exposing (Id, Uri)
import Json.Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias Library =
    { id : Maybe Id
    , url : Maybe Uri
    , name : Maybe String
    , title : Maybe String
    , content : List Attachment
    }


new : String -> Library
new url =
    { id = Nothing
    , url = Just url
    , name = Nothing
    , title = Nothing
    , content = []
    }


encode : Library -> Value
encode { id, url, name, title, content } =
    Encode.object <|
        ( "resourceType", Encode.string "Library" )
            :: List.filterMap identity
                [ Maybe.map (\s -> ( "id", Encode.string s )) id
                , Maybe.map (\s -> ( "url", Encode.string s )) url
                , Maybe.map (\s -> ( "name", Encode.string s )) name
                , Maybe.map (\s -> ( "title", Encode.string s )) title
                , Just ( "content", Encode.list Attachment.encode content )
                ]


decoder : Decoder Library
decoder =
    succeed Library
        |> optional "id" (maybe string) Nothing
        |> optional "url" (maybe string) Nothing
        |> optional "name" (maybe string) Nothing
        |> optional "title" (maybe string) Nothing
        |> optional "content" (list Attachment.decoder) []
