module Page.Settings exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , view
    )

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class, classList)
import Json.Encode exposing (encode)
import List.Zipper as Zipper exposing (Zipper)
import Material.Button as Button
import Material.Card as Card
import Material.IconButton as IconButton
import Material.List as List
import Material.List.Item as ListItem exposing (ListItem)
import Material.Radio as Radio
import NaturalOrdering
import Page.Settings.ServerDialog as ServerDialog
import Ports
import Session exposing (Server, Session)



-- MODEL


type alias Model =
    { session : Session
    , serverDialog : ServerDialog.Model
    , onServerSave : Maybe (Server -> Msg)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , serverDialog = ServerDialog.init
      , onServerSave = Nothing
      }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ClickedAddServer
    | ClickedServerSaveAtAdd Server
    | ClickedServerSaveAtEdit Server Server
    | ClickedServer String
    | ClickedServerEdit Server
    | ClickedServerDelete String
    | GotServerDialogMsg ServerDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedAddServer ->
            ( { model
                | serverDialog =
                    ServerDialog.doOpen Session.emptyServer model.serverDialog
                , onServerSave = Just ClickedServerSaveAtAdd
              }
            , Cmd.none
            )

        ClickedServerSaveAtAdd server ->
            let
                activeServer =
                    Session.activeServer model.session

                session =
                    updateServers (addServer server activeServer) model.session
            in
            ( { model
                | session = session
                , serverDialog = ServerDialog.init
                , onServerSave = Nothing
              }
            , storeSession session
            )

        ClickedServerSaveAtEdit oldServer newServer ->
            let
                session =
                    updateServers (updateServer oldServer newServer) model.session
            in
            ( { model
                | session = session
                , serverDialog = ServerDialog.init
                , onServerSave = Nothing
              }
            , storeSession session
            )

        ClickedServer name ->
            let
                session =
                    updateServers (activateServer name) model.session
            in
            ( { model | session = session }
            , storeSession session
            )

        ClickedServerEdit server ->
            ( { model
                | serverDialog =
                    ServerDialog.doOpen server model.serverDialog
                , onServerSave = Just (ClickedServerSaveAtEdit server)
              }
            , Cmd.none
            )

        ClickedServerDelete name ->
            let
                session =
                    updateServers (deleteServer name) model.session
            in
            ( { model | session = session }
            , storeSession session
            )

        GotServerDialogMsg msg_ ->
            ( { model
                | serverDialog = ServerDialog.update msg_ model.serverDialog
              }
            , Cmd.none
            )


addServer : Server -> Server -> Zipper Server -> Zipper Server
addServer server activeServer servers =
    let
        add list =
            List.sortBy .name (server :: list)
    in
    case compareServer server activeServer of
        LT ->
            Zipper.mapBefore add servers

        EQ ->
            servers

        GT ->
            Zipper.mapAfter add servers


compareServer : Server -> Server -> Order
compareServer =
    NaturalOrdering.compareOn .name


updateServers : (Zipper Server -> Zipper Server) -> Session -> Session
updateServers f session =
    { session | servers = f session.servers }


activateServer : String -> Zipper Server -> Zipper Server
activateServer name servers =
    Zipper.findFirst (.name >> (==) name) servers |> Maybe.withDefault servers


updateServer : Server -> Server -> Zipper Server -> Zipper Server
updateServer oldServer newServer servers =
    let
        tryUpdate server =
            if server == oldServer then
                newServer

            else
                server
    in
    servers
        |> Zipper.mapBefore (List.map tryUpdate)
        |> Zipper.mapCurrent tryUpdate
        |> Zipper.mapAfter (List.map tryUpdate)


deleteServer : String -> Zipper Server -> Zipper Server
deleteServer name servers =
    Zipper.from
        (List.filter (.name >> (/=) name) (Zipper.before servers))
        (Zipper.current servers)
        (List.filter (.name >> (/=) name) (Zipper.after servers))


storeSession : Session -> Cmd msg
storeSession session =
    Ports.storeSession (encode 0 (Session.encode session))



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    { title = [ "Settings" ]
    , content =
        div [ class "main-content settings-page" ]
            [ viewServerDialog model
            , serverListCard model.session.servers
            ]
    }


viewServerDialog : Model -> Html Msg
viewServerDialog model =
    ServerDialog.view
        { onMsg = GotServerDialogMsg
        , onSave = model.onServerSave
        }
        model.serverDialog


serverListCard : Zipper Server -> Html Msg
serverListCard servers =
    Card.card
        (Card.config
            |> Card.setOutlined True
            |> Card.setAttributes [ class "settings-server-list-card" ]
        )
        { blocks =
            [ Card.block <|
                div [ class "settings-server-list-card__header" ]
                    [ h2 [ class "mdc-typography--headline6" ]
                        [ text "Servers" ]
                    ]
            , Card.block <|
                div [ class "settings-server-list-card__body" ]
                    [ serverList servers ]
            ]
        , actions =
            Just <|
                Card.actions
                    { buttons =
                        [ Card.button
                            (Button.config
                                |> Button.setOnClick ClickedAddServer
                            )
                            "add"
                        ]
                    , icons = []
                    }
        }


serverList : Zipper Server -> Html Msg
serverList servers =
    List.list List.config <|
        List.map serverListItem (toMarkedList servers)


toMarkedList : Zipper a -> List ( Bool, a )
toMarkedList zipper =
    List.map (Tuple.pair False) (Zipper.before zipper)
        ++ [ ( True, Zipper.current zipper ) ]
        ++ List.map (Tuple.pair False) (Zipper.after zipper)


serverListItem : ( Bool, Server ) -> ListItem Msg
serverListItem ( active, { name } as server ) =
    ListItem.listItem
        (ListItem.config
            |> ListItem.setAttributes
                [ classList [ ( "settings-server-list-item--active", active ) ] ]
        )
        [ ListItem.graphic []
            [ Radio.radio
                (Radio.config
                    |> Radio.setChecked active
                    |> Radio.setOnChange (ClickedServer name)
                )
            ]
        , text name
        , ListItem.meta []
            [ IconButton.iconButton
                (IconButton.config
                    |> IconButton.setOnClick (ClickedServerEdit server)
                )
                "edit"
            , if active then
                text ""

              else
                IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setOnClick (ClickedServerDelete name)
                    )
                    "delete"
            ]
        ]
