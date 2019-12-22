module Page.Measure.Sidebar.Subject exposing (Model, Msg, init, update, view)

{-| This component is the subject input box in the sidebar.
-}

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
import Events exposing (onEnterEsc)
import Html exposing (text)
import Material.Button exposing (buttonConfig, outlinedButton, unelevatedButton)
import Material.TextField exposing (textField, textFieldConfig)



-- MODEL


type alias Model =
    { originalSubject : String
    , enteredSubject : String
    , edit : Bool
    }


init : String -> Model
init subject =
    { originalSubject = subject
    , enteredSubject = subject
    , edit = False
    }



-- UPDATE


type Msg
    = ClickedEdit
    | ClickedCancel
    | EnteredSubject String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedEdit ->
            ( { model | edit = True }, Cmd.none )

        ClickedCancel ->
            ( { model | enteredSubject = model.originalSubject, edit = False }
            , Cmd.none
            )

        EnteredSubject s ->
            ( { model | enteredSubject = s }, Cmd.none )



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : String -> msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { onMsg, onSave } { enteredSubject, edit } =
    sidebarEntry sidebarEntryConfig
        [ sidebarEntryTitle []
            [ text "Subject"
            , sidebarEditButton
                { sidebarEditButtonConfig | onClick = Just (onMsg ClickedEdit) }
            ]
        , sidebarEntryContent [] <|
            if edit then
                [ textField
                    { textFieldConfig
                        | value = enteredSubject
                        , onInput = Just (EnteredSubject >> onMsg)
                        , outlined = True
                        , additionalAttributes =
                            [ onEnterEsc
                                (onSave enteredSubject)
                                (onMsg ClickedCancel)
                            ]
                    }
                ]

            else
                [ text enteredSubject ]
        , sidebarEntryActionButtons [] <|
            if edit then
                [ unelevatedButton
                    { buttonConfig | onClick = Just (onSave enteredSubject) }
                    "save"
                , outlinedButton
                    { buttonConfig | onClick = Just (onMsg ClickedCancel) }
                    "cancel"
                ]

            else
                []
        ]
