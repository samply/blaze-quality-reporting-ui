module Page.Settings.ServerDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Events exposing (onEnter)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.TextField as TextField
import MaterialUtil
import Maybe.Extra as MaybeExtra
import Session exposing (Server)


type Model
    = Closed
    | Open { server : Server }


init : Model
init =
    Closed



-- UPDATE


type Msg
    = ClickedClose
    | EnteredName String
    | EnteredUrl String


doOpen : Server -> Model -> Model
doOpen server _ =
    Open { server = server }


doClose : Model -> Model
doClose _ =
    Closed


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedClose ->
            doClose model

        EnteredName name ->
            updateServer (\server -> { server | name = name }) model

        EnteredUrl url ->
            updateServer (\server -> { server | url = url }) model


updateServer f model =
    case model of
        Open data ->
            Open { data | server = f data.server }

        Closed ->
            model



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe (Server -> msg)
    }


view : Config msg -> Model -> Html msg
view { onMsg, onSave } model =
    let
        server =
            case model of
                Open data ->
                    Just data.server

                Closed ->
                    Nothing

        name =
            Maybe.map .name server

        url =
            Maybe.map .url server

        onSave_ =
            MaybeExtra.andMap server onSave
    in
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen (isOpen model)
            |> Dialog.setOnClose (onMsg ClickedClose)
            |> Dialog.setAttributes [ class "settings-server-dialog" ]
        )
        { title = Just "Server"
        , content =
            [ nameField onSave_ onMsg name
            , urlField onSave_ onMsg url
            ]
        , actions =
            [ Button.text
                (Button.config
                    |> Button.setOnClick (onMsg ClickedClose)
                )
                "Cancel"
            , Button.text
                (Button.config
                    |> MaterialUtil.liftMaybe Button.setOnClick onSave_
                    |> Button.setDisabled (isEmpty name || isEmpty url)
                )
                "Save"
            ]
        }


isOpen model =
    case model of
        Open _ ->
            True

        Closed ->
            False


isEmpty : Maybe String -> Bool
isEmpty s =
    s
        |> Maybe.map String.isEmpty
        |> Maybe.withDefault True


nameField onSave onMsg name =
    TextField.filled
        (TextField.config
            |> TextField.setLabel (Just "Name")
            |> TextField.setValue name
            |> TextField.setOnInput (EnteredName >> onMsg)
            |> TextField.setRequired True
            |> TextField.setValid
                (Maybe.map (String.isEmpty >> not) name
                    |> Maybe.withDefault False
                )
            |> TextField.setAttributes
                (onSave
                    |> Maybe.map onEnter
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
                )
        )


urlField onSave onMsg url =
    TextField.filled
        (TextField.config
            |> TextField.setLabel (Just "URL")
            |> TextField.setValue url
            |> TextField.setOnInput (EnteredUrl >> onMsg)
            |> TextField.setRequired True
            |> TextField.setValid
                (Maybe.map (String.isEmpty >> not) url
                    |> Maybe.withDefault False
                )
            |> TextField.setAttributes
                (onSave
                    |> Maybe.map onEnter
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
                )
        )
