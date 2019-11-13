module Fhir.Measure exposing
    ( Group
    , Measure
    , Population
    , Stratifier
    , decoder
    , encode
    , measurePopulationType
    , measureScoring
    , new
    )

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Fhir.Coding exposing (Coding)
import Fhir.Encode exposing (object, optionalListPair, optionalPair, pair)
import Fhir.Expression as Expression exposing (Expression)
import Fhir.PrimitiveTypes exposing (Canonical, Id, Uri)
import Json.Decode exposing (Decoder, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as MaybeExtra


type alias Measure =
    { id : Maybe Id
    , name : Maybe String
    , title : Maybe String
    , subtitle : Maybe String
    , subject : Maybe CodeableConcept
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
    , criteria : Expression
    }


type alias Stratifier =
    { code : Maybe CodeableConcept
    , description : Maybe String
    , criteria : Maybe Expression
    }


new : Maybe Uri -> Measure
new libraryUrl =
    { id = Nothing
    , name = Nothing
    , title = Nothing
    , subtitle = Nothing
    , subject =
        Just
            { coding =
                [ { system = Just "http://hl7.org/fhir/resource-types"
                  , version = Nothing
                  , code = Just "Patient"
                  }
                ]
            , text = Nothing
            }
    , library = MaybeExtra.toList libraryUrl
    , scoring = Nothing
    , group =
        [ { code = Nothing
          , description = Nothing
          , population =
                [ { code = Nothing
                  , description = Nothing
                  , criteria =
                        { language = "text/cql"
                        , expression = Just "InInitialPopulation"
                        }
                  }
                ]
          , stratifier = []
          }
        ]
    }


measureScoring : String -> Coding
measureScoring code =
    { system = Just "http://terminology.hl7.org/CodeSystem/measure-scoring"
    , version = Nothing
    , code = Just code
    }


measurePopulationType : String -> Coding
measurePopulationType code =
    { system = Just "http://terminology.hl7.org/CodeSystem/measure-population"
    , version = Nothing
    , code = Just code
    }


encode : Measure -> Value
encode { id, name, title, subtitle, subject, library, scoring, group } =
    object
        [ pair "resourceType" Encode.string "Measure"
        , optionalPair "id" Encode.string id
        , optionalPair "name" Encode.string name
        , optionalPair "title" Encode.string title
        , optionalPair "subtitle" Encode.string subtitle
        , optionalPair "subjectCodeableConcept" CodeableConcept.encode subject
        , optionalListPair "library" Encode.string library
        , optionalPair "scoring" CodeableConcept.encode scoring
        , optionalListPair "group" encodeGroup group
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
        , pair "criteria" Expression.encode criteria
        ]


encodeStratifier : Stratifier -> Value
encodeStratifier { code, description, criteria } =
    object
        [ optionalPair "code" CodeableConcept.encode code
        , optionalPair "description" Encode.string description
        , optionalPair "criteria" Expression.encode criteria
        ]


decoder : Decoder Measure
decoder =
    succeed Measure
        |> optional "id" (maybe string) Nothing
        |> optional "name" (maybe string) Nothing
        |> optional "title" (maybe string) Nothing
        |> optional "subtitle" (maybe string) Nothing
        |> optional "subjectCodeableConcept" (maybe CodeableConcept.decoder) Nothing
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
        |> required "criteria" Expression.decoder


stratifierDecoder : Decoder Stratifier
stratifierDecoder =
    succeed Stratifier
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> optional "description" (maybe string) Nothing
        |> optional "criteria" (maybe Expression.decoder) Nothing
