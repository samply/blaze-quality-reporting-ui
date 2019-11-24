module Fhir.MeasureReport exposing
    ( Group
    , MeasureReport
    , Population
    , Status(..)
    , Stratifier
    , Stratum
    , Type(..)
    , decoder
    )

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Fhir.Extension as Extension exposing (Extension)
import Fhir.MeasureReport.Stratum as Stratum
import Fhir.PrimitiveTypes exposing (Canonical, DateTime, Id, Markdown, Uri)
import Json.Decode exposing (Decoder, andThen, fail, int, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


type alias MeasureReport =
    { id : Maybe Id
    , extension : List Extension
    , status : Status
    , type_ : Type
    , measure : Canonical
    , date : Maybe DateTime
    , group : List Group
    }


type Status
    = Complete
    | Pending
    | Error


type Type
    = Individual
    | SubjectList
    | Summary
    | DataCollection


type alias Group =
    { code : Maybe CodeableConcept
    , population : List Population
    , stratifier : List Stratifier
    }


type alias Population =
    { code : Maybe CodeableConcept
    , count : Maybe Int
    }


type alias Stratifier =
    { code : List CodeableConcept
    , stratum : List Stratum
    }


type alias Stratum =
    { value : Maybe CodeableConcept
    , component : List Stratum.Component
    , population : List Population
    }


decoder : Decoder MeasureReport
decoder =
    succeed MeasureReport
        |> optional "id" (maybe string) Nothing
        |> optional "extension" (list Extension.decoder) []
        |> required "status" statusDecoder
        |> required "type" typeDecoder
        |> required "measure" string
        |> optional "date" (maybe string) Nothing
        |> optional "group" (list groupDecoder) []


statusDecoder : Decoder Status
statusDecoder =
    andThen
        (\s ->
            case s of
                "complete" ->
                    succeed Complete

                "pending" ->
                    succeed Pending

                "error" ->
                    succeed Error

                _ ->
                    fail ("Invalid status `" ++ s ++ "`.")
        )
        string


typeDecoder : Decoder Type
typeDecoder =
    andThen
        (\s ->
            case s of
                "individual" ->
                    succeed Individual

                "subject-list" ->
                    succeed SubjectList

                "summary" ->
                    succeed Summary

                "data-collection" ->
                    succeed DataCollection

                _ ->
                    fail ("Invalid type `" ++ s ++ "`.")
        )
        string


groupDecoder : Decoder Group
groupDecoder =
    succeed Group
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> optional "population" (list populationDecoder) []
        |> optional "stratifier" (list stratifierDecoder) []


populationDecoder : Decoder Population
populationDecoder =
    succeed Population
        |> optional "code" (maybe CodeableConcept.decoder) Nothing
        |> optional "count" (maybe int) Nothing


stratifierDecoder : Decoder Stratifier
stratifierDecoder =
    succeed Stratifier
        |> optional "code" (list CodeableConcept.decoder) []
        |> optional "stratum" (list stratumDecoder) []


stratumDecoder : Decoder Stratum
stratumDecoder =
    succeed Stratum
        |> optional "value" (maybe CodeableConcept.decoder) Nothing
        |> optional "component" (list Stratum.componentDecoder) []
        |> optional "population" (list populationDecoder) []
