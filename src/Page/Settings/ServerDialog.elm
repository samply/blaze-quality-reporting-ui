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
import Material.Button exposing (buttonConfig, textButton)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.TextField exposing (textField, textFieldConfig)
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
    dialog
        { dialogConfig
            | open = isOpen model
            , onClose = Just (onMsg ClickedClose)
            , additionalAttributes = [ class "settings-server-dialog" ]
        }
        { title = Just "Server"
        , content =
            [ nameField onSave_ onMsg name
            , urlField onSave_ onMsg url
            ]
        , actions =
            [ textButton
                { buttonConfig
                    | onClick = Just (onMsg ClickedClose)
                }
                "Cancel"
            , textButton
                { buttonConfig
                    | onClick = onSave_
                    , disabled = isEmpty name || isEmpty url
                }
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
    textField
        { textFieldConfig
            | label = Just "Name"
            , value = Maybe.withDefault "" name
            , onInput = Just (EnteredName >> onMsg)
            , required = True
            , valid =
                Maybe.map (String.isEmpty >> not) name
                    |> Maybe.withDefault False
            , additionalAttributes =
                onSave
                    |> Maybe.map onEnter
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
        }


urlField onSave onMsg url =
    textField
        { textFieldConfig
            | label = Just "URL"
            , value = Maybe.withDefault "" url
            , onInput = Just (EnteredUrl >> onMsg)
            , required = True
            , valid =
                Maybe.map (String.isEmpty >> not) url
                    |> Maybe.withDefault False
            , additionalAttributes =
                onSave
                    |> Maybe.map onEnter
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
        }
