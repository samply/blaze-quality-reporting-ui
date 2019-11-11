module Measure.LoadDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Html, text)
import Http as Http
import Json.Decode exposing (decodeValue)
import List
import Material.Dialog exposing (dialog, dialogConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Process
import Task
import Url.Builder as UrlBuilder


type alias Model =
    { base : String
    , open : Bool
    , measures : Maybe (List Measure)
    , search : String
    }


init base =
    { base = base
    , open = False
    , measures = Nothing
    , search = ""
    }


type Msg
    = Close
    | ChangeSearch String
    | MaybePerformSearch String
    | Select Measure
    | ListResult (Result Http.Error Bundle)


doOpen : Model -> ( Model, Cmd Msg )
doOpen model =
    ( { model
        | open = True
      }
    , searchMeasures model ""
    )


doClose : Model -> Model
doClose model =
    { model | open = False }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Close ->
            ( { model | open = False }, Cmd.none )

        ChangeSearch search ->
            ( { model | search = search }
            , Process.sleep 200
                |> Task.map (\_ -> search)
                |> Task.perform MaybePerformSearch
            )

        MaybePerformSearch search ->
            if search == model.search then
                ( model, searchMeasures model search )

            else
                ( model, Cmd.none )

        Select _ ->
            ( model, Cmd.none )

        ListResult (Ok bundle) ->
            if bundle.type_ == "searchset" then
                ( { model | measures = Just (decodeMeasures bundle) }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ListResult (Err _) ->
            ( model, Cmd.none )


searchMeasures { base } query =
    let
        params =
            if String.isEmpty query then
                []

            else
                [ UrlBuilder.string "title:contains" query ]
    in
    FhirHttp.searchType ListResult base "Measure" params


decodeMeasures : Bundle -> List Measure
decodeMeasures { entry } =
    List.filterMap
        (.resource >> decodeValue Measure.decoder >> Result.toMaybe)
        entry


type alias Config msg =
    { onMsg : Msg -> msg
    , onSelect : Measure -> msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onSelect } { open, measures, search } =
    dialog
        { dialogConfig
            | open = open
            , onClose = Just (onMsg Close)
        }
        { title = Just "Load Measure"
        , content =
            [ textField
                { textFieldConfig
                    | placeholder = Just "Search"
                    , value = Just search
                    , onInput = Just (ChangeSearch >> onMsg)
                    , fullwidth = True
                }
            , measureList onSelect <| Maybe.withDefault [] measures
            ]
        , actions = []
        }


measureList onSelect measures =
    list listConfig <|
        List.map (measureListItem onSelect) measures


measureListItem onSelect measure =
    listItem { listItemConfig | onClick = Just <| onSelect measure }
        [ text <| measureTitle measure ]


measureTitle { title, name } =
    List.filterMap identity [ title, name ]
        |> List.head
        |> Maybe.withDefault "<unknown>"
