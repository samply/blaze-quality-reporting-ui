module Component.Sidebar.SharePanel exposing (Model, Msg, init, update, view)

import Component.Sidebar
    exposing
        ( SidebarEntry
        , sidebarEntry
        , sidebarEntryConfig
        , sidebarEntryContent
        , sidebarEntryTitle
        )
import Component.Sidebar.SharePanel.CopyToServerDialog as CopyToServerDialog
import Html exposing (text)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Session exposing (Server)



-- MODEL


type alias Model =
    { copyToServerDialog : CopyToServerDialog.Model }


init : Model
init =
    { copyToServerDialog = CopyToServerDialog.init }



-- UPDATE


type Msg
    = ClickedCopyToServer
    | GotCopyToServerDialogMsg CopyToServerDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCopyToServer ->
            updateCopyToServerDialog CopyToServerDialog.doOpen model

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
    sidebarEntry sidebarEntryConfig
        [ viewCopyToServerDialog servers onMsg onCopyToServer copyToServerDialog
        , sidebarEntryTitle []
            [ text "Share"
            ]
        , sidebarEntryContent []
            [ iconButton
                { iconButtonConfig | onClick = Just (onMsg ClickedCopyToServer) }
                "share"
            ]
        ]


viewCopyToServerDialog servers onMsg onCopyToServer copyToServerDialog =
    CopyToServerDialog.view
        { servers = servers
        , onMsg = GotCopyToServerDialogMsg >> onMsg
        , onSelect = onCopyToServer
        }
        copyToServerDialog
