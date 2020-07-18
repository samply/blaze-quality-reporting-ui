module Page.Library.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar as Sidebar
import Component.Sidebar.SharePanel as SharePanel
import Component.Sidebar.SubjectPanel as SubjectPanel
import Component.Sidebar.UrlPanel as UrlPanel
import Component.Sidebar.VersionPanel as VersionPanel
import Fhir.Library as Library exposing (Library)
import Html exposing (Html)
import Session exposing (Server)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { library : Library
    , urlPanel : UrlPanel.Model
    , versionPanel : VersionPanel.Model
    , subjectPanel : SubjectPanel.Model
    , sharePanel : SharePanel.Model
    }


init : String -> Library -> Model
init base library =
    let
        apiUrl =
            UrlBuilder.crossOrigin base [ "Library", library.id ] []
    in
    { library = library
    , urlPanel = UrlPanel.init library.url
    , versionPanel = VersionPanel.init library.version
    , subjectPanel = SubjectPanel.init (Library.getSubjectCode library)
    , sharePanel = SharePanel.init apiUrl
    }



-- UPDATE


type Msg
    = GotUrlMsg UrlPanel.Msg
    | GotVersionMsg VersionPanel.Msg
    | GotSubjectMsg SubjectPanel.Msg
    | GotShareMsg SharePanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUrlMsg msg_ ->
            let
                ( url, cmd ) =
                    UrlPanel.update msg_ model.urlPanel
            in
            ( { model | urlPanel = url }, Cmd.map GotUrlMsg cmd )

        GotVersionMsg msg_ ->
            let
                ( version, cmd ) =
                    VersionPanel.update msg_ model.versionPanel
            in
            ( { model | versionPanel = version }, Cmd.map GotVersionMsg cmd )

        GotSubjectMsg msg_ ->
            let
                ( subject, cmd ) =
                    SubjectPanel.update msg_ model.subjectPanel
            in
            ( { model | subjectPanel = subject }, Cmd.map GotSubjectMsg cmd )

        GotShareMsg msg_ ->
            let
                ( share, cmd ) =
                    SharePanel.update msg_ model.sharePanel
            in
            ( { model | sharePanel = share }, Cmd.map GotShareMsg cmd )



-- VIEW


type alias Config msg =
    { servers : List Server
    , onMsg : Msg -> msg
    , onSave : Library -> msg
    , onCopyToServer : Server -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    Sidebar.view Sidebar.config
        [ viewUrlPanel config model
        , viewVersionPanel config model
        , viewSubjectPanel config model
        , viewSharePanel config model
        ]


viewUrlPanel { onMsg, onSave } { library, urlPanel } =
    UrlPanel.view
        { onMsg = GotUrlMsg >> onMsg
        , onSave = \url -> onSave { library | url = url }
        }
        urlPanel


viewVersionPanel { onMsg, onSave } { library, versionPanel } =
    VersionPanel.view
        { onMsg = GotVersionMsg >> onMsg
        , onSave = \version -> onSave { library | version = version }
        }
        versionPanel


viewSubjectPanel { onMsg, onSave } { library, subjectPanel } =
    SubjectPanel.view
        { onMsg = GotSubjectMsg >> onMsg
        , onSave = \code -> onSave (Library.setSubjectCode code library)
        }
        subjectPanel


viewSharePanel { servers, onMsg, onCopyToServer } { sharePanel } =
    SharePanel.view
        { servers = servers
        , onMsg = GotShareMsg >> onMsg
        , onCopyToServer = onCopyToServer
        }
        sharePanel
