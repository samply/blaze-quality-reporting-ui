module Page.Measure.AssocLibraryDialog exposing
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
import Fhir.Library as Library exposing (Library)
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


type Model
    = Closed { base : String }
    | Open
        { base : String
        , libraries : List Library
        , search : String
        }


init base =
    Closed { base = base }


type Msg
    = ClickedClose
    | EnteredSearch String
    | MaybePerformSearch String
    | CompletedLoad (Result Http.Error Bundle)


doOpen : Model -> ( Model, Cmd Msg )
doOpen model =
    case model of
        Open _ ->
            ( model, Cmd.none )

        Closed { base } ->
            ( Open { base = base, libraries = [], search = "" }
            , searchLibraries base ""
            )


doClose : Model -> Model
doClose model =
    case model of
        Open { base } ->
            Closed { base = base }

        Closed { base } ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedClose, _ ) ->
            ( doClose model, Cmd.none )

        ( EnteredSearch search, Open data ) ->
            ( Open { data | search = search }
            , Process.sleep 200
                |> Task.map (\_ -> search)
                |> Task.perform MaybePerformSearch
            )

        ( MaybePerformSearch search, Open data ) ->
            if search == data.search then
                ( model, searchLibraries data.base search )

            else
                ( model, Cmd.none )

        ( CompletedLoad (Ok bundle), Open data ) ->
            if bundle.type_ == "searchset" then
                ( Open { data | libraries = decodeLibraries bundle }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


searchLibraries base query =
    let
        params =
            if String.isEmpty query then
                []

            else
                [ UrlBuilder.string "title:contains" query ]
    in
    FhirHttp.searchType CompletedLoad base "Library" params


decodeLibraries : Bundle -> List Library
decodeLibraries { entry } =
    List.filterMap
        (.resource >> decodeValue Library.decoder >> Result.toMaybe)
        entry


type alias Config msg =
    { onMsg : Msg -> msg
    , onSelect : Library -> msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onSelect } model =
    let
        ( open, search, libraries ) =
            case model of
                Open data ->
                    ( True, data.search, data.libraries )

                Closed _ ->
                    ( False, "", [] )
    in
    dialog
        { dialogConfig
            | open = open
            , onClose = Just (onMsg ClickedClose)
        }
        { title = Just "Load Library"
        , content =
            [ textField
                { textFieldConfig
                    | placeholder = Just "Search"
                    , value = search
                    , onInput = Just (EnteredSearch >> onMsg)
                    , fullwidth = True
                }
            , if List.isEmpty libraries then
                emptyListPlaceholder

              else
                libraryList onSelect libraries
            ]
        , actions = []
        }


emptyListPlaceholder =
    text "no libraries available"


libraryList onSelect libraries =
    list listConfig <|
        List.map (libraryListItem onSelect) libraries


libraryListItem onSelect library =
    listItem { listItemConfig | onClick = Just <| onSelect library }
        [ text <| libraryTitle library ]


libraryTitle { title, name, url } =
    List.filterMap identity [ title, name, url ]
        |> List.head
        |> Maybe.withDefault "<unknown>"
