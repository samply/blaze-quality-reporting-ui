module Component.Sidebar.SharePanel exposing (Model, Msg, init, update, view)

import Component.Button as Button
import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Component.Sidebar.SharePanel.CopyToServerDialog as CopyToServerDialog
import Ports
import Session exposing (Server)



-- MODEL


type alias Model =
    { apiUrl : String
    , copyToServerDialog : CopyToServerDialog.Model
    }


init : String -> Model
init apiUrl =
    { apiUrl = apiUrl
    , copyToServerDialog = CopyToServerDialog.init
    }



-- UPDATE


type Msg
    = ClickedCopyToServer
    | ClickedCopyApiUrlToClipboard
    | GotCopyToServerDialogMsg CopyToServerDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCopyToServer ->
            updateCopyToServerDialog CopyToServerDialog.doOpen model

        ClickedCopyApiUrlToClipboard ->
            ( model, Ports.writeToClipboard model.apiUrl )

        GotCopyToServerDialogMsg msg_ ->
            updateCopyToServerDialog (CopyToServerDialog.update msg_) model


updateCopyToServerDialog f model =
    let
        ( dialog, cmd ) =
            f model.copyToServerDialog
    in
    ( { model | copyToServerDialog = dialog }
    , Cmd.map GotCopyToServerDialogMsg cmd
    )



-- VIEW


type alias Config msg =
    { servers : List Server
    , onMsg : Msg -> msg
    , onCopyToServer : Server -> msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { servers, onMsg, onCopyToServer } { copyToServerDialog } =
    SidebarEntry.view SidebarEntry.config
        [ viewCopyToServerDialog servers onMsg onCopyToServer copyToServerDialog
        , SidebarEntry.actionButtons []
            [ Button.secondary
                (Button.config
                    |> Button.setOnClick (onMsg ClickedCopyToServer)
                )
                "Share"
            , Button.secondary
                (Button.config
                    |> Button.setOnClick (onMsg ClickedCopyApiUrlToClipboard)
                )
                "Copy URL"
            ]
        ]


viewCopyToServerDialog servers onMsg onCopyToServer copyToServerDialog =
    CopyToServerDialog.view
        { servers = servers
        , onMsg = GotCopyToServerDialogMsg >> onMsg
        , onSelect = onCopyToServer
        }
        copyToServerDialog
