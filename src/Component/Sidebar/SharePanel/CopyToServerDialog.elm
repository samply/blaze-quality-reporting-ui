module Component.Sidebar.SharePanel.CopyToServerDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Component.Dialog as Dialog
import Component.List as List
import Component.List.Item as ListItem
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Session exposing (Server)



-- MODEL


type Model
    = Closed
    | Open


init : Model
init =
    Closed



-- UPDATE


type Msg
    = ClickedClose


doOpen : Model -> ( Model, Cmd Msg )
doOpen model =
    case model of
        Open ->
            ( model, Cmd.none )

        Closed ->
            ( Open, Cmd.none )


doClose : Model -> Model
doClose model =
    case model of
        Open ->
            Closed

        Closed ->
            Open


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedClose ->
            ( doClose model, Cmd.none )



-- VIEW


type alias Config msg =
    { servers : List Server
    , onMsg : Msg -> msg
    , onSelect : Server -> msg
    }


view : Config msg -> Model -> Html msg
view { servers, onMsg, onSelect } model =
    let
        open =
            case model of
                Open ->
                    True

                Closed ->
                    False
    in
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen open
            |> Dialog.setOnClose (onMsg ClickedClose)
            |> Dialog.setAttributes [ class "copy-to-server-dialog" ]
        )
        { title = Just "Copy to Server"
        , content =
            [ serverList onSelect servers
            ]
        , actions = []
        }


serverList onSelect servers =
    if List.isEmpty servers then
        text "no servers available"

    else
        List.list List.config <|
            List.map (serverListItem onSelect) servers


serverListItem onSelect server =
    ListItem.listItem
        (ListItem.config |> ListItem.setOnClick (onSelect server))
        server.url
        [ ListItem.text []
            { primary = [ text server.name ]
            , secondary = [ text server.url ]
            }
        ]
