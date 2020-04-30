module Page.Library.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar exposing (sidebar, sidebarConfig)
import Component.Sidebar.SharePanel as SharePanel
import Component.Sidebar.UrlPanel as UrlPanel
import Fhir.Library exposing (Library)
import Html exposing (Html)
import Session exposing (Server)



-- MODEL


type alias Model =
    { library : Library
    , sharePanel : SharePanel.Model
    , urlPanel : UrlPanel.Model
    }


init : Library -> Model
init library =
    { library = library
    , sharePanel = SharePanel.init
    , urlPanel = UrlPanel.init library.url
    }



-- UPDATE


type Msg
    = GotShareMsg SharePanel.Msg
    | GotUrlMsg UrlPanel.Msg


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



-- VIEW


type alias Config msg =
    { servers : List Server
    , onMsg : Msg -> msg
    , onSave : Library -> msg
    , onCopyToServer : Server -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    sidebar sidebarConfig
        [ viewSharePanel config model
        , viewUrlPanel config model
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
