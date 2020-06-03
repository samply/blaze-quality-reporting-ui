module Page.Library.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar as Sidebar
import Component.Sidebar.SharePanel as SharePanel
import Component.Sidebar.UrlPanel as UrlPanel
import Component.Sidebar.VersionPanel as VersionPanel
import Fhir.Library exposing (Library)
import Html exposing (Html)
import Session exposing (Server)



-- MODEL


type alias Model =
    { library : Library
    , sharePanel : SharePanel.Model
    , urlPanel : UrlPanel.Model
    , versionPanel : VersionPanel.Model
    }


init : Library -> Model
init library =
    { library = library
    , sharePanel = SharePanel.init
    , urlPanel = UrlPanel.init library.url
    , versionPanel = VersionPanel.init library.version
    }



-- UPDATE


type Msg
    = GotShareMsg SharePanel.Msg
    | GotUrlMsg UrlPanel.Msg
    | GotVersionMsg VersionPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotShareMsg msg_ ->
            let
                ( share, cmd ) =
                    SharePanel.update msg_ model.sharePanel
            in
            ( { model | sharePanel = share }, Cmd.map GotShareMsg cmd )

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
        [ viewSharePanel config model
        , viewUrlPanel config model
        , viewVersionPanel config model
        ]


viewSharePanel { servers, onMsg, onCopyToServer } { sharePanel } =
    SharePanel.view
        { servers = servers
        , onMsg = GotShareMsg >> onMsg
        , onCopyToServer = onCopyToServer
        }
        sharePanel


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
