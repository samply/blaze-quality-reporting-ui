module Page.Library.List exposing (Model, Msg, init, toSession, update, view)

import Fhir.Bundle exposing (Bundle)
import Fhir.CodeableConcept as CodeableConcept
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Material.Button exposing (buttonConfig, textButton)
import Material.Fab exposing (fab, fabConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig)
import Route
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { session : Session
    , libraries : Status (List Library)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , libraries = Loading
      }
    , searchLibraries session.base ""
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
            ( model, createLibrary model.session.base )

        CompletedLoadLibraries (Ok bundle) ->
            ( { model | libraries = Loaded (decodeLibraries bundle) }
            , Cmd.none
            )

        CompletedLoadLibraries (Err _) ->
            ( { model | libraries = Failed }
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
    Route.pushUrl (Session.navKey model.session) (Route.Library id)


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
            , name = Nothing
            , title = Nothing
            , status = Library.Draft
            , type_ = CodeableConcept.ofOneCoding (Library.type_ "logic-library")
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
                if List.isEmpty libraries then
                    emptyListPlaceholder

                else
                    libraryList libraries

            Loading ->
                text ""

            LoadingSlowly ->
                text ""

            Failed ->
                text "error"
    }


emptyListPlaceholder =
    div [ class "main-content library-list__empty-placeholder" ]
        [ textButton
            { buttonConfig | onClick = Just ClickedCreateLibrary }
            "create the first library"
        ]


libraryList libraries =
    div [ class "main-content library-list" ]
        [ createButton
        , list listConfig <|
            List.map libraryListItem libraries
        ]


libraryListItem library =
    listItem { listItemConfig | onClick = Just <| ClickedLibrary library }
        [ text <| libraryTitle library ]


libraryTitle { title, name, id } =
    List.filterMap identity [ title, name, id ]
        |> List.head
        |> Maybe.withDefault "<unknown>"


createButton =
    fab
        { fabConfig
            | onClick = Just ClickedCreateLibrary
            , additionalAttributes = [ class "library-list__create-button" ]
        }
        "add"
