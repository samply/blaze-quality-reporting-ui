module Fhir.Library exposing
    ( Library
    , decoder
    , encode
    , getSubjectCode
    , setSubjectCode
    , type_
    )

import Fhir.Attachment as Attachment exposing (Attachment)
import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Fhir.Coding exposing (Coding)
import Fhir.Encode exposing (object, optionalListPair, optionalPair, pair)
import Fhir.Meta as Meta exposing (Meta)
import Fhir.PrimitiveTypes exposing (Id, Markdown, Uri)
import Fhir.Reference as Reference exposing (Reference)
import Fhir.ValueSet.PublicationStatus as PublicationStatus exposing (PublicationStatus)
import Json.Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Util


type alias Library =
    { id : Id
    , meta : Maybe Meta
    , url : Maybe Uri
    , version : Maybe String
    , name : Maybe String
    , title : Maybe String
    , status : PublicationStatus
    , type_ : CodeableConcept
    , subjectCodeableConcept : Maybe CodeableConcept
    , subjectReference : Maybe Reference
    , description : Maybe Markdown
    , content : List Attachment
    }


type_ : String -> Coding
type_ code =
    { system = Just "http://terminology.hl7.org/CodeSystem/library-type"
    , version = Nothing
    , code = Just code
    }


getSubjectCode : Library -> String
getSubjectCode =
    .subjectCodeableConcept
        >> Maybe.andThen getResourceTypeCode
        >> Maybe.withDefault "Patient"


getResourceTypeCode =
    CodeableConcept.getCodeOf "http://hl7.org/fhir/resource-types"


setSubjectCode : Maybe String -> Library -> Library
setSubjectCode code measure =
    case code of
        Just c ->
            case measure.subjectCodeableConcept of
                Just subject ->
                    { measure | subjectCodeableConcept = Just (setResourceTypeCode c subject) }

                Nothing ->
                    { measure | subjectCodeableConcept = Just (subjectCodeableConcept c) }

        Nothing ->
            { measure | subjectCodeableConcept = Nothing }


setResourceTypeCode =
    CodeableConcept.setCodeOf "http://hl7.org/fhir/resource-types"


subjectCodeableConcept code =
    CodeableConcept.ofOneCoding
        { system = Just "http://hl7.org/fhir/resource-types"
        , version = Nothing
        , code = Just code
        }


encode : Library -> Value
encode library =
    object
        [ pair "resourceType" Encode.string "Library"
        , optionalPair "id" Encode.string <| Util.emptyToNothing library.id
        , optionalPair "meta" Meta.encode library.meta
        , optionalPair "url" Encode.string library.url
        , optionalPair "version" Encode.string library.version
        , optionalPair "name" Encode.string library.name
        , optionalPair "title" Encode.string library.title
        , pair "status" PublicationStatus.encode library.status
        , pair "type" CodeableConcept.encode library.type_
        , optionalPair "subjectCodeableConcept" CodeableConcept.encode library.subjectCodeableConcept
        , optionalPair "subjectReference" Reference.encode library.subjectReference
        , optionalPair "description" Encode.string library.description
        , optionalListPair "content" Attachment.encode library.content
        ]


decoder : Decoder Library
decoder =
    succeed Library
        |> required "id" string
        |> optional "meta" (maybe Meta.decoder) Nothing
        |> optional "url" (maybe string) Nothing
        |> optional "version" (maybe string) Nothing
        |> optional "name" (maybe string) Nothing
        |> optional "title" (maybe string) Nothing
        |> optional "status" PublicationStatus.decoder PublicationStatus.Unknown
        |> optional "type" CodeableConcept.decoder CodeableConcept.empty
        |> optional "subjectCodeableConcept" (maybe CodeableConcept.decoder) Nothing
        |> optional "subjectReference" (maybe Reference.decoder) Nothing
        |> optional "description" (maybe string) Nothing
        |> optional "content" (list Attachment.decoder) []
