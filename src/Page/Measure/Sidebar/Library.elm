module Page.Measure.Sidebar.Library exposing (Model, Msg, init, update, view)

import Component.Sidebar
    exposing
        ( SidebarEntry
        , sidebarEditButton
        , sidebarEditButtonConfig
        , sidebarEntry
        , sidebarEntryConfig
        , sidebarEntryContent
        , sidebarEntryTitle
        )
import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (Html, a, text)
import Json.Decode exposing (decodeValue)
import Route exposing (href)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { url : Maybe Canonical
    , library : Maybe Library
    }


init : String -> Maybe Canonical -> ( Model, Cmd Msg )
init base url =
    ( { url = url
      , library = Nothing
      }
    , Maybe.map (searchLibrary base) url |> Maybe.withDefault Cmd.none
    )



-- UPDATE


type Msg
    = CompletedSearch (Result FhirHttp.Error Bundle)


update : Msg -> Model -> Model
update msg model =
    case msg of
        CompletedSearch (Ok bundle) ->
            case decodeLibraries bundle of
                firstLibrary :: otherLibraries ->
                    if List.isEmpty otherLibraries then
                        { model | library = Just firstLibrary }

                    else
                        model

                _ ->
                    model

        CompletedSearch (Err _) ->
            model


searchLibrary base url =
    FhirHttp.searchType CompletedSearch
        base
        "Library"
        [ UrlBuilder.string "url" url ]


decodeLibraries : Bundle -> List Library
decodeLibraries { entry } =
    List.filterMap
        (.resource >> decodeValue Library.decoder >> Result.toMaybe)
        entry



-- VIEW


type alias Config msg =
    { onEdit : msg }


view : Config msg -> Model -> SidebarEntry msg
view { onEdit } model =
    sidebarEntry sidebarEntryConfig
        [ sidebarEntryTitle []
            [ text "Library"
            , sidebarEditButton
                { sidebarEditButtonConfig | onClick = Just onEdit }
            ]
        , sidebarEntryContent []
            [ libraryLink model ]
        ]


libraryLink : Model -> Html msg
libraryLink model =
    case model.url of
        Just url ->
            case Maybe.andThen .id model.library of
                Just id ->
                    a [ href (Route.Library id) ]
                        [ model.library
                            |> Maybe.andThen libraryTitle
                            |> Maybe.withDefault url
                            |> text
                        ]

                Nothing ->
                    model.library
                        |> Maybe.andThen libraryTitle
                        |> Maybe.withDefault url
                        |> text

        Nothing ->
            text "No associated library"


libraryTitle { title, name, id } =
    List.filterMap identity [ title, name, id ]
        |> List.head
