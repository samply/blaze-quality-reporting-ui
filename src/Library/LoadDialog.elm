module Library.LoadDialog exposing
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


type alias Model =
    { base : String
    , open : Bool
    , libraries : Maybe (List Library)
    , search : String
    }


init base =
    { base = base
    , open = False
    , libraries = Nothing
    , search = ""
    }


type Msg
    = Close
    | ChangeSearch String
    | MaybePerformSearch String
    | Select Library
    | ListResult (Result Http.Error Bundle)


doOpen : Model -> ( Model, Cmd Msg )
doOpen model =
    ( { model
        | open = True
      }
    , searchLibraries model ""
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
                ( model, searchLibraries model search )

            else
                ( model, Cmd.none )

        Select _ ->
            ( model, Cmd.none )

        ListResult (Ok bundle) ->
            if bundle.type_ == "searchset" then
                ( { model | libraries = Just (decodeLibraries bundle) }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ListResult (Err _) ->
            ( model, Cmd.none )


searchLibraries { base } query =
    let
        params =
            if String.isEmpty query then
                []

            else
                [ UrlBuilder.string "title:contains" query ]
    in
    FhirHttp.searchType ListResult base "Library" params


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
view { onMsg, onSelect } { open, libraries, search } =
    dialog
        { dialogConfig
            | open = open
            , onClose = Just (onMsg Close)
        }
        { title = Just "Load Library"
        , content =
            [ textField
                { textFieldConfig
                    | placeholder = Just "Search"
                    , value = Just search
                    , onInput = Just (ChangeSearch >> onMsg)
                    , fullwidth = True
                }
            , libraryList onSelect <| Maybe.withDefault [] libraries
            ]
        , actions = []
        }


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
