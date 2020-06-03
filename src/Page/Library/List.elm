module Page.Library.List exposing (Model, Msg, init, toSession, update, view)

import Component.Error as Error
import Fhir.Bundle exposing (Bundle)
import Fhir.CodeableConcept as CodeableConcept
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Loading exposing (Status(..))
import Material.Button as Button
import Material.Fab as Fab
import Material.List as List
import Material.List.Item as ListItem
import Route
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { session : Session
    , libraries : Status (List Library)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , libraries = Loading
      }
    , searchLibraries (Session.getBase session) ""
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ClickedLibrary Library
    | ClickedCreateLibrary
    | CompletedLoadLibraries (Result FhirHttp.Error Bundle)
    | CompletedCreateLibrary (Result FhirHttp.Error Library)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLibrary library ->
            ( model
            , library.id
                |> Maybe.map (pushLibraryUrl model)
                |> Maybe.withDefault Cmd.none
            )

        ClickedCreateLibrary ->
            ( model, createLibrary (Session.getBase model.session) )

        CompletedLoadLibraries (Ok bundle) ->
            ( { model | libraries = Loaded (decodeLibraries bundle) }
            , Cmd.none
            )

        CompletedLoadLibraries (Err error) ->
            ( { model | libraries = Failed error }
            , Cmd.none
            )

        CompletedCreateLibrary (Ok library) ->
            ( model
            , library.id
                |> Maybe.map (pushLibraryUrl model)
                |> Maybe.withDefault Cmd.none
            )

        CompletedCreateLibrary (Err _) ->
            ( model, Cmd.none )


pushLibraryUrl model id =
    Route.pushUrl (Session.toNavKey model.session) (Route.Library id)


searchLibraries base query =
    let
        params =
            if String.isEmpty query then
                []

            else
                [ UrlBuilder.string "title:contains" query ]
    in
    FhirHttp.searchType CompletedLoadLibraries base "Library" params


decodeLibraries : Bundle -> List Library
decodeLibraries { entry } =
    List.filterMap
        (.resource >> decodeValue Library.decoder >> Result.toMaybe)
        entry


createLibrary : String -> Cmd Msg
createLibrary base =
    let
        library =
            { id = Nothing
            , url = Nothing
            , version = Nothing
            , name = Nothing
            , title = Nothing
            , status = Library.Draft
            , type_ = CodeableConcept.ofOneCoding (Library.type_ "logic-library")
            , subjectCodeableConcept = Nothing
            , subjectReference = Nothing
            , description = Nothing
            , content = []
            }
    in
    FhirHttp.create
        CompletedCreateLibrary
        base
        "Library"
        Library.decoder
        (Library.encode library)



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    { title = [ "Libraries" ]
    , content =
        case model.libraries of
            Loaded libraries ->
                div [ class "main-content library-list-page" ]
                    [ if List.isEmpty libraries then
                        emptyListPlaceholder

                      else
                        libraryList libraries
                    ]

            Loading ->
                text ""

            LoadingSlowly ->
                text ""

            Failed error ->
                div [ class "main-content library-list-page" ]
                    [ Error.view error ]
    }


emptyListPlaceholder =
    div [ class "library-list-page__empty-placeholder" ]
        [ Button.text
            (Button.config |> Button.setOnClick ClickedCreateLibrary)
            "create the first library"
        ]


libraryList libraries =
    div [ class "library-list-page__list" ]
        [ createButton
        , List.list (List.config |> List.setTwoLine True) <|
            List.map libraryListItem libraries
        ]


libraryListItem library =
    ListItem.listItem
        (ListItem.config |> ListItem.setOnClick (ClickedLibrary library))
        [ ListItem.text []
            { primary = [ text <| libraryTitle library ]
            , secondary = [ text <| libraryDetails library ]
            }
        ]


libraryTitle { title, name, id } =
    List.filterMap identity [ title, name, id ]
        |> List.head
        |> Maybe.withDefault "<unknown>"


libraryDetails { subjectCodeableConcept } =
    subjectCodeableConcept
        |> Maybe.andThen (CodeableConcept.getCodeOf "http://hl7.org/fhir/resource-types")
        |> Maybe.withDefault "Patient"


createButton =
    Fab.fab
        (Fab.config
            |> Fab.setOnClick ClickedCreateLibrary
            |> Fab.setAttributes [ class "library-list-page__create-button" ]
        )
        "add"
