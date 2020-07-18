module Page.Measure.Sidebar.LibraryPanel exposing (Model, Msg, init, update, view)

import Component.Button as Button
import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Fhir.PrimitiveTypes exposing (Canonical)
import Html exposing (Html, a, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Loading exposing (Status(..))
import Route exposing (href)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { url : Maybe Canonical
    , library : Status Library
    }


init : String -> Maybe Canonical -> ( Model, Cmd Msg )
init base url =
    ( { url = url
      , library = Loading
      }
    , Maybe.map (searchLibrary base) url
        |> Maybe.withDefault Cmd.none
    )



-- UPDATE


type Msg
    = CompletedSearch (Result FhirHttp.Error Bundle)
    | PassedSlowLoadingThreshold


update : Msg -> Model -> Model
update msg model =
    case msg of
        CompletedSearch (Ok bundle) ->
            case decodeLibraries bundle of
                [ onlyLibrary ] ->
                    { model | library = Loaded onlyLibrary }

                _ ->
                    model

        CompletedSearch (Err error) ->
            { model | library = Failed error }

        PassedSlowLoadingThreshold ->
            { model | library = Loading.markLoadingSlowly model.library }


searchLibrary base url =
    Cmd.batch
        [ FhirHttp.searchType CompletedSearch
            base
            "Library"
            [ UrlBuilder.string "url" url ]
        , Loading.slowThreshold PassedSlowLoadingThreshold
        ]


decodeLibraries : Bundle -> List Library
decodeLibraries { entry } =
    List.filterMap
        (.resource
            >> Maybe.map (decodeValue Library.decoder)
            >> Maybe.andThen Result.toMaybe
        )
        entry



-- VIEW


type alias Config msg =
    { onEdit : msg }


view : Config msg -> Model -> SidebarEntry msg
view { onEdit } model =
    SidebarEntry.view SidebarEntry.config
        [ SidebarEntry.title []
            [ text "Library"
            , SidebarEntry.editButton
                (Button.config |> Button.setOnClick onEdit)
            ]
        , SidebarEntry.content [ class "truncate" ]
            [ libraryLink model ]
        ]


libraryLink : Model -> Html msg
libraryLink model =
    case model.url of
        Just url ->
            case model.library of
                LoadingSlowly ->
                    text url

                Loaded library ->
                    a [ href (Route.Library library.id) ]
                        [ text <| libraryTitle library ]

                Reloading library ->
                    a [ href (Route.Library library.id) ]
                        [ text <| libraryTitle library ]

                ReloadingSlowly library ->
                    a [ href (Route.Library library.id) ]
                        [ text <| libraryTitle library ]

                _ ->
                    Html.span [ class "invisible" ] [ text url ]

        Nothing ->
            text "No associated library"


libraryTitle { title, name, id } =
    List.filterMap identity [ title, name ]
        |> List.head
        |> Maybe.withDefault id
