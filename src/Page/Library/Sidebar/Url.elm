module Page.Library.Sidebar.Url exposing (Model, Msg, init, update, view)

import Component.Sidebar
    exposing
        ( SidebarEntry
        , sidebarEditButton
        , sidebarEditButtonConfig
        , sidebarEntry
        , sidebarEntryActionButtons
        , sidebarEntryConfig
        , sidebarEntryContent
        , sidebarEntryTitle
        )
import Fhir.PrimitiveTypes exposing (Uri)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Material.Button exposing (buttonConfig, outlinedButton, unelevatedButton)
import Material.TextField exposing (textField, textFieldConfig)



-- model


type alias Model =
    { url : Maybe Uri
    , edit : Bool
    }


init : Maybe Uri -> Model
init url =
    { url = url
    , edit = False
    }



-- UPDATE


type Msg
    = ClickedEdit
    | ClickedCancel
    | EnteredUrl String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedEdit ->
            { model | edit = True }

        ClickedCancel ->
            { model | edit = False }

        EnteredUrl s ->
            { model | url = Just s }



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe Uri -> msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { onMsg, onSave } { url, edit } =
    sidebarEntry sidebarEntryConfig
        [ sidebarEntryTitle []
            [ text "URL"
            , sidebarEditButton
                { sidebarEditButtonConfig | onClick = Just (onMsg ClickedEdit) }
            ]
        , sidebarEntryContent [] <|
            if edit then
                [ textField
                    { textFieldConfig
                        | value = url |> Maybe.withDefault ""
                        , onInput = Just (EnteredUrl >> onMsg)
                        , outlined = True
                    }
                ]

            else
                [ url |> Maybe.withDefault "<not-specified>" |> text ]
        , sidebarEntryActionButtons [] <|
            if edit then
                [ unelevatedButton
                    { buttonConfig | onClick = Just (onSave url) }
                    "save"
                , outlinedButton
                    { buttonConfig
                        | onClick = Just (onMsg ClickedCancel)
                        , additionalAttributes = [ style "margin-left" "0.5rem" ]
                    }
                    "cancel"
                ]

            else
                []
        ]
