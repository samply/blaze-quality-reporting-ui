module Page.Measure.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar as Sidebar
import Component.Sidebar.SharePanel as SharePanel
import Component.Sidebar.SubjectPanel as SubjectPanel
import Component.Sidebar.UrlPanel as UrlPanel
import Component.Sidebar.VersionPanel as VersionPanel
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Html)
import Page.Measure.Sidebar.LibraryPanel as LibraryPanel
import Route exposing (Route)
import Session exposing (Server)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { measure : Measure
    , urlPanel : UrlPanel.Model
    , versionPanel : VersionPanel.Model
    , subjectPanel : SubjectPanel.Model
    , libraryPanel : LibraryPanel.Model
    , sharePanel : SharePanel.Model
    }


init : String -> Measure -> ( Model, Cmd Msg )
init base measure =
    let
        ( libraryPanel, cmd ) =
            LibraryPanel.init base (List.head measure.library)

        apiUrl =
            UrlBuilder.crossOrigin base [ "Measure", measure.id ] []
    in
    ( { measure = measure
      , urlPanel = UrlPanel.init measure.url
      , versionPanel = VersionPanel.init measure.version
      , subjectPanel = SubjectPanel.init (Measure.getSubjectCode measure)
      , libraryPanel = libraryPanel
      , sharePanel = SharePanel.init apiUrl
      }
    , Cmd.map GotLibraryMsg cmd
    )



-- UPDATE


type Msg
    = GotShareMsg SharePanel.Msg
    | GotUrlMsg UrlPanel.Msg
    | GotVersionMsg VersionPanel.Msg
    | GotSubjectMsg SubjectPanel.Msg
    | GotLibraryMsg LibraryPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Route )
update msg model =
    case msg of
        GotShareMsg msg_ ->
            let
                ( share, cmd ) =
                    SharePanel.update msg_ model.sharePanel
            in
            ( { model | sharePanel = share }
            , Cmd.map GotShareMsg cmd
            , Nothing
            )

        GotUrlMsg msg_ ->
            let
                ( url, cmd ) =
                    UrlPanel.update msg_ model.urlPanel
            in
            ( { model | urlPanel = url }
            , Cmd.map GotUrlMsg cmd
            , Nothing
            )

        GotVersionMsg msg_ ->
            let
                ( version, cmd ) =
                    VersionPanel.update msg_ model.versionPanel
            in
            ( { model | versionPanel = version }
            , Cmd.map GotVersionMsg cmd
            , Nothing
            )

        GotSubjectMsg msg_ ->
            let
                ( subject, cmd ) =
                    SubjectPanel.update msg_ model.subjectPanel
            in
            ( { model | subjectPanel = subject }
            , Cmd.map GotSubjectMsg cmd
            , Nothing
            )

        GotLibraryMsg panelMsg ->
            let
                ( panel, maybeRoute ) =
                    LibraryPanel.update panelMsg model.libraryPanel
            in
            ( { model | libraryPanel = panel }
            , Cmd.none
            , maybeRoute
            )



-- VIEW


type alias Config msg =
    { servers : List Server
    , onMsg : Msg -> msg
    , onSave : Measure -> msg
    , onLibraryEdit : msg
    , onCopyToServer : Server -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    Sidebar.view Sidebar.config
        [ viewUrlPanel config model
        , viewVersionPanel config model
        , viewSubjectPanel config model
        , viewLibraryPanel config model
        , viewSharePanel config model
        ]


viewSharePanel { servers, onMsg, onCopyToServer } { sharePanel } =
    SharePanel.view
        { servers = servers
        , onMsg = GotShareMsg >> onMsg
        , onCopyToServer = onCopyToServer
        }
        sharePanel


viewUrlPanel { onMsg, onSave } { measure, urlPanel } =
    UrlPanel.view
        { onMsg = GotUrlMsg >> onMsg
        , onSave = \url -> onSave { measure | url = url }
        }
        urlPanel


viewVersionPanel { onMsg, onSave } { measure, versionPanel } =
    VersionPanel.view
        { onMsg = GotVersionMsg >> onMsg
        , onSave = \version -> onSave { measure | version = version }
        }
        versionPanel


viewSubjectPanel { onMsg, onSave } { measure, subjectPanel } =
    SubjectPanel.view
        { onMsg = GotSubjectMsg >> onMsg
        , onSave = \code -> onSave (Measure.setSubjectCode code measure)
        }
        subjectPanel


viewLibraryPanel { onMsg, onLibraryEdit } { libraryPanel } =
    LibraryPanel.view
        { onMsg = GotLibraryMsg >> onMsg
        , onEdit = onLibraryEdit
        }
        libraryPanel
