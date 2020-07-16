module Page.Settings.ServerDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Component.Button as Button
import Component.Dialog as Dialog
import Component.TextField as TextField
import Events exposing (onEnter)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Maybe.Extra as MaybeExtra
import Session exposing (Server)
import Util


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
        )
        { title = Just "Server"
        , content =
            [ nameField onSave_ onMsg name
            , urlField onSave_ onMsg url
            ]
        , actions =
            [ Button.secondary
                (Button.config
                    |> Button.setOnClick (onMsg ClickedClose)
                )
                "Cancel"
            , Button.primary
                (Button.config
                    |> Util.liftMaybe Button.setOnClick onSave_
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
    Html.div [ class "mb-2" ]
        [ Html.div [ class "mb-1" ] [ Html.text "Name" ]
        , TextField.outlined
            (TextField.config
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
        ]


urlField onSave onMsg url =
    Html.div []
        [ Html.div [ class "mb-1" ] [ Html.text "URL" ]
        , TextField.outlined
            (TextField.config
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
        ]
