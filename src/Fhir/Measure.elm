module Fhir.Measure exposing (Group, Measure, Population, decoder, encode, new)

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
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
    , subject : Maybe CodeableConcept
    , library : List Canonical
    , scoring : Maybe CodeableConcept
    , group : List Group
    }


type alias Group =
    { population : List Population }


type alias Population =
    { code : Maybe CodeableConcept
    , criteria : Expression
    }


new : Maybe Uri -> Measure
new libraryUrl =
    { id = Nothing
    , name = Nothing
    , title = Nothing
    , subject =
        Just
            { coding =
                [ { system = Just "http://hl7.org/fhir/resource-types"
                  , version = Nothing
                  , code = Just "Patient"
                  }
                ]
            }
    , library = MaybeExtra.toList libraryUrl
    , scoring =
        Just
            { coding =
                [ { system = Just "http://terminology.hl7.org/CodeSystem/measure-scoring"
                  , version = Nothing
                  , code = Just "cohort"
                  }
                ]
            }
    , group =
        [ { population =
                [ { code =
                        Just
                            { coding =
                                [ { system = Just "http://terminology.hl7.org/CodeSystem/measure-population"
                                  , version = Nothing
                                  , code = Just "initial-population"
                                  }
                                ]
                            }
                  , criteria =
                        { language = "text/cql"
                        , expression = Just "InInitialPopulation"
                        }
                  }
                ]
          }
        ]
    }


encode : Measure -> Value
encode { id, name, title, subject, library, scoring, group } =
    Encode.object <|
        ( "resourceType", Encode.string "Measure" )
            :: List.filterMap identity
                [ Maybe.map (\s -> ( "id", Encode.string s )) id
                , Maybe.map (\s -> ( "name", Encode.string s )) name
                , Maybe.map (\s -> ( "title", Encode.string s )) title
                , Maybe.map (\s -> ( "subjectCodeableConcept", CodeableConcept.encode s )) subject
                , Just ( "library", Encode.list Encode.string library )
                , Maybe.map (\s -> ( "scoring", CodeableConcept.encode s )) scoring
                , Just ( "group", Encode.list encodeGroup group )
                ]


encodeGroup : Group -> Value
encodeGroup { population } =
    Encode.object
        [ ( "population", Encode.list encodePopulation population ) ]


encodePopulation : Population -> Value
encodePopulation { code, criteria } =
    Encode.object <|
        List.filterMap identity
            [ Maybe.map (\s -> ( "code", CodeableConcept.encode s )) code
            , Just ( "criteria", Expression.encode criteria )
            ]


decoder : Decoder Measure
decoder =
    succeed Measure
        |> optional "id" (maybe string) Nothing
        |> optional "name" (maybe string) Nothing
        |> optional "title" (maybe string) Nothing
        |> optional "subjectCodeableConcept" (maybe CodeableConcept.decoder) Nothing
        |> optional "library" (list string) []
        |> optional "scoring" (maybe CodeableConcept.decoder) Nothing
        |> optional "group" (list groupDecoder) []


groupDecoder : Decoder Group
groupDecoder =
    succeed Group
        |> optional "population" (list populationDecoder) []


populationDecoder : Decoder Population
populationDecoder =
    succeed Population
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> required "criteria" Expression.decoder
