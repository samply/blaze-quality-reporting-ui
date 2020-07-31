module Fhir.Measure exposing
    ( Group
    , Measure
    , Population
    , Stratifier
    , decoder
    , encode
    , getSubjectCode
    , newStratifier
    , populationType
    , scoring
    , setSubjectCode
    )

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Fhir.Coding exposing (Coding)
import Fhir.Encode exposing (object, optionalListPair, optionalPair, pair)
import Fhir.Expression as Expression exposing (Expression)
import Fhir.Measure.Stratifier as Stratifier
import Fhir.Meta as Meta exposing (Meta)
import Fhir.PrimitiveTypes exposing (Canonical, Id, Markdown, Uri)
import Fhir.ValueSet.PublicationStatus as PublicationStatus exposing (PublicationStatus)
import Json.Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Util


type alias Measure =
    { id : Id
    , meta : Maybe Meta
    , url : Maybe Uri
    , version : Maybe String
    , name : Maybe String
    , title : Maybe String
    , subtitle : Maybe String
    , status : PublicationStatus
    , subject : Maybe CodeableConcept
    , description : Maybe Markdown
    , library : List Canonical
    , scoring : Maybe CodeableConcept
    , group : List Group
    }


type alias Group =
    { code : Maybe CodeableConcept
    , description : Maybe String
    , population : List Population
    , stratifier : List Stratifier
    }


type alias Population =
    { code : Maybe CodeableConcept
    , description : Maybe String
    , criteria : Maybe Expression
    }


type alias Stratifier =
    { code : Maybe CodeableConcept
    , description : Maybe String
    , criteria : Maybe Expression
    , component : List Stratifier.Component
    }


scoring : String -> Coding
scoring code =
    { system = Just "http://terminology.hl7.org/CodeSystem/measure-scoring"
    , version = Nothing
    , code = Just code
    }


populationType : String -> Coding
populationType code =
    { system = Just "http://terminology.hl7.org/CodeSystem/measure-population"
    , version = Nothing
    , code = Just code
    }


newStratifier =
    { code = Nothing
    , description = Nothing
    , criteria = Just (Expression.cql Nothing)
    , component = []
    }


getSubjectCode : Measure -> String
getSubjectCode =
    .subject >> Maybe.andThen getResourceTypeCode >> Maybe.withDefault "Patient"


getResourceTypeCode =
    CodeableConcept.getCodeOf "http://hl7.org/fhir/resource-types"


setSubjectCode : Maybe String -> Measure -> Measure
setSubjectCode code measure =
    case code of
        Just c ->
            case measure.subject of
                Just subject ->
                    { measure | subject = Just (setResourceTypeCode c subject) }

                Nothing ->
                    { measure | subject = Just (subjectCodeableConcept c) }

        Nothing ->
            { measure | subject = Nothing }


setResourceTypeCode =
    CodeableConcept.setCodeOf "http://hl7.org/fhir/resource-types"


subjectCodeableConcept code =
    CodeableConcept.ofOneCoding
        { system = Just "http://hl7.org/fhir/resource-types"
        , version = Nothing
        , code = Just code
        }


encode : Measure -> Value
encode measure =
    object
        [ pair "resourceType" Encode.string "Measure"
        , optionalPair "id" Encode.string <| Util.emptyToNothing measure.id
        , optionalPair "meta" Meta.encode measure.meta
        , optionalPair "url" Encode.string measure.url
        , optionalPair "version" Encode.string measure.version
        , optionalPair "name" Encode.string measure.name
        , optionalPair "title" Encode.string measure.title
        , optionalPair "subtitle" Encode.string measure.subtitle
        , pair "status" PublicationStatus.encode measure.status
        , optionalPair "subjectCodeableConcept" CodeableConcept.encode measure.subject
        , optionalPair "description" Encode.string measure.description
        , optionalListPair "library" Encode.string measure.library
        , optionalPair "scoring" CodeableConcept.encode measure.scoring
        , optionalListPair "group" encodeGroup measure.group
        ]


encodeGroup : Group -> Value
encodeGroup { code, description, population, stratifier } =
    object
        [ optionalPair "code" CodeableConcept.encode code
        , optionalPair "description" Encode.string description
        , optionalListPair "population" encodePopulation population
        , optionalListPair "stratifier" encodeStratifier stratifier
        ]


encodePopulation : Population -> Value
encodePopulation { code, description, criteria } =
    object
        [ optionalPair "code" CodeableConcept.encode code
        , optionalPair "description" Encode.string description
        , optionalPair "criteria" Expression.encode criteria
        ]


encodeStratifier : Stratifier -> Value
encodeStratifier { code, description, criteria, component } =
    object
        [ optionalPair "code" CodeableConcept.encode code
        , optionalPair "description" Encode.string description
        , optionalPair "criteria" Expression.encode criteria
        , optionalListPair "component" Stratifier.encodeComponent component
        ]


decoder : Decoder Measure
decoder =
    succeed Measure
        |> required "id" string
        |> optional "meta" (maybe Meta.decoder) Nothing
        |> optional "url" (maybe string) Nothing
        |> optional "version" (maybe string) Nothing
        |> optional "name" (maybe string) Nothing
        |> optional "title" (maybe string) Nothing
        |> optional "subtitle" (maybe string) Nothing
        |> optional "status" PublicationStatus.decoder PublicationStatus.Unknown
        |> optional "subjectCodeableConcept" (maybe CodeableConcept.decoder) Nothing
        |> optional "description" (maybe string) Nothing
        |> optional "library" (list string) []
        |> optional "scoring" (maybe CodeableConcept.decoder) Nothing
        |> optional "group" (list groupDecoder) []


groupDecoder : Decoder Group
groupDecoder =
    succeed Group
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> optional "description" (maybe string) Nothing
        |> optional "population" (list populationDecoder) []
        |> optional "stratifier" (list stratifierDecoder) []


populationDecoder : Decoder Population
populationDecoder =
    succeed Population
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> optional "description" (maybe string) Nothing
        |> optional "criteria" (maybe Expression.decoder) Nothing


stratifierDecoder : Decoder Stratifier
stratifierDecoder =
    succeed Stratifier
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> optional "description" (maybe string) Nothing
        |> optional "criteria" (maybe Expression.decoder) Nothing
        |> optional "component" (list Stratifier.componentDecoder) []
