module Fhir.Library exposing (Library, Status(..), decoder, encode, type_)

import Fhir.Attachment as Attachment exposing (Attachment)
import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Fhir.Coding exposing (Coding)
import Fhir.Encode exposing (object, optionalListPair, optionalPair, pair)
import Fhir.PrimitiveTypes exposing (Id, Markdown, Uri)
import Json.Decode as Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode exposing (Value)


type alias Library =
    { id : Maybe Id
    , url : Maybe Uri
    , name : Maybe String
    , title : Maybe String
    , status : Status
    , type_ : CodeableConcept
    , description : Maybe Markdown
    , content : List Attachment
    }


type Status
    = Draft
    | Active
    | Retired
    | Unknown


type_ : String -> Coding
type_ code =
    { system = Just "http://terminology.hl7.org/CodeSystem/library-type"
    , version = Nothing
    , code = Just code
    }


encode : Library -> Value
encode library =
    object
        [ pair "resourceType" Encode.string "Library"
        , optionalPair "id" Encode.string library.id
        , optionalPair "url" Encode.string library.url
        , optionalPair "name" Encode.string library.name
        , optionalPair "title" Encode.string library.title
        , pair "status" encodeStatus library.status
        , pair "type" CodeableConcept.encode library.type_
        , optionalPair "description" Encode.string library.description
        , optionalListPair "content" Attachment.encode library.content
        ]


encodeStatus status =
    case status of
        Draft ->
            Encode.string "draft"

        Active ->
            Encode.string "active"

        Retired ->
            Encode.string "retired"

        Unknown ->
            Encode.string "unknown"


decoder : Decoder Library
decoder =
    succeed Library
        |> optional "id" (maybe string) Nothing
        |> optional "url" (maybe string) Nothing
        |> optional "name" (maybe string) Nothing
        |> optional "title" (maybe string) Nothing
        |> optional "status" statusDecoder Unknown
        |> optional "type" CodeableConcept.decoder CodeableConcept.empty
        |> optional "description" (maybe string) Nothing
        |> optional "content" (list Attachment.decoder) []


statusDecoder : Decoder Status
statusDecoder =
    Decode.map
        (\s ->
            case s of
                "draft" ->
                    Draft

                "active" ->
                    Active

                "retired" ->
                    Retired

                _ ->
                    Unknown
        )
        string
