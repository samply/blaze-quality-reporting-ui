module Page.Measure.Sidebar.LibraryPanel exposing (Model, Msg, init, update, view)

import Component.Button as Button
import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Loading exposing (Status(..))
import Route exposing (Route)
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
    = ClickedLibrary Id
    | CompletedSearch (Result FhirHttp.Error Bundle)
    | PassedSlowLoadingThreshold


update : Msg -> Model -> ( Model, Maybe Route )
update msg model =
    case msg of
        ClickedLibrary id ->
            ( model, Just (Route.Library id) )

        CompletedSearch (Ok bundle) ->
            case decodeLibraries bundle of
                [ onlyLibrary ] ->
                    ( { model | library = Loaded onlyLibrary }, Nothing )

                _ ->
                    ( model, Nothing )

        CompletedSearch (Err error) ->
            ( { model | library = Failed error }, Nothing )

        PassedSlowLoadingThreshold ->
            ( { model | library = Loading.markLoadingSlowly model.library }
            , Nothing
            )


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
    { onMsg : Msg -> msg
    , onEdit : msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { onMsg, onEdit } model =
    SidebarEntry.view SidebarEntry.config
        [ SidebarEntry.title []
            [ text "Library"
            , SidebarEntry.editButton
                (Button.config |> Button.setOnClick onEdit)
            ]
        , SidebarEntry.content [ class "truncate" ]
            [ libraryLink onMsg model ]
        ]


libraryLink : (Msg -> msg) -> Model -> Html msg
libraryLink onMsg model =
    case model.url of
        Just url ->
            case model.library of
                LoadingSlowly ->
                    text url

                Loaded library ->
                    Button.text
                        (Button.config
                            |> Button.setOnClick (onMsg (ClickedLibrary library.id))
                        )
                        (libraryTitle library)

                Reloading library ->
                    Button.text
                        (Button.config
                            |> Button.setOnClick (onMsg (ClickedLibrary library.id))
                        )
                        (libraryTitle library)

                ReloadingSlowly library ->
                    Button.text
                        (Button.config
                            |> Button.setOnClick (onMsg (ClickedLibrary library.id))
                        )
                        (libraryTitle library)

                _ ->
                    Html.span [ class "invisible" ] [ text url ]

        Nothing ->
            text "No associated library"


libraryTitle { title, name, id } =
    List.filterMap identity [ title, name ]
        |> List.head
        |> Maybe.withDefault id
