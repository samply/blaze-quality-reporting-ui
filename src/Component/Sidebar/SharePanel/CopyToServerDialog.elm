module Component.Sidebar.SharePanel.CopyToServerDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.List
    exposing
        ( list
        , listConfig
        , listItem
        , listItemConfig
        , listItemPrimaryText
        , listItemSecondaryText
        , listItemText
        )
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
            ( Open
            , Cmd.none
            )


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
    dialog
        { dialogConfig
            | open = open
            , onClose = Just (onMsg ClickedClose)
            , additionalAttributes = [ class "copy-to-server-dialog" ]
        }
        { title = Just "Copy to Server"
        , content =
            [ if List.isEmpty servers then
                emptyListPlaceholder

              else
                serverList onSelect servers
            ]
        , actions = []
        }


emptyListPlaceholder =
    text "no libraries available"


serverList onSelect servers =
    list { listConfig | twoLine = True } <|
        List.map (serverListItem onSelect) servers


serverListItem onSelect server =
    listItem
        { listItemConfig
            | onClick = Just <| onSelect server
        }
        [ listItemText []
            [ listItemPrimaryText []
                [ text server.name ]
            , listItemSecondaryText []
                [ text server.url ]
            ]
        ]
