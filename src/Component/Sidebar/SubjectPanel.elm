module Component.Sidebar.SubjectPanel exposing (Model, Msg, init, update, view)

{-| This component is the subject input box in the sidebar.
-}

import Browser.Dom as Dom
import Component.Button as Button
import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Component.TextField as TextField
import Html exposing (text)
import Html.Attributes exposing (class)
import Task
import Util



-- MODEL


type alias Model =
    { originalSubject : String
    , enteredSubject : Maybe String
    , edit : Bool
    , valid : Bool
    }


init : String -> Model
init subject =
    { originalSubject = subject
    , enteredSubject = Just subject
    , edit = False
    , valid = not (String.isEmpty subject)
    }



-- UPDATE


type Msg
    = ClickedEdit
    | ClickedCancel
    | TextFieldFocused (Result Dom.Error ())
    | EnteredSubject String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedEdit ->
            ( { model | edit = True }
            , Task.attempt TextFieldFocused (Dom.focus "subject")
            )

        ClickedCancel ->
            ( { model
                | enteredSubject = Just model.originalSubject
                , edit = False
              }
            , Cmd.none
            )

        TextFieldFocused _ ->
            ( model, Cmd.none )

        EnteredSubject subject ->
            ( { model
                | enteredSubject = Just subject
                , valid = not (String.isEmpty subject)
              }
            , Cmd.none
            )



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe String -> msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { onMsg, onSave } { originalSubject, enteredSubject, edit, valid } =
    SidebarEntry.view SidebarEntry.config
        [ SidebarEntry.title []
            [ text "Subject"
            , SidebarEntry.editButton
                (Button.config |> Button.setOnClick (onMsg ClickedEdit))
            ]
        , SidebarEntry.content [] <|
            if edit then
                [ TextField.outlined
                    (TextField.config
                        |> TextField.setId (Just "subject")
                        |> TextField.setValue enteredSubject
                        |> TextField.setRequired True
                        |> TextField.setValid valid
                        |> TextField.setOnInput (EnteredSubject >> onMsg)
                        |> Util.applyIf valid (TextField.setOnEnter (onSave enteredSubject))
                        |> TextField.setOnEsc (onMsg ClickedCancel)
                    )
                ]

            else
                [ text originalSubject ]
        , SidebarEntry.actionButtons [] <|
            if edit then
                [ saveButton (not valid) (onSave enteredSubject)
                , cancelButton (onMsg ClickedCancel)
                ]

            else
                []
        ]


saveButton disabled onClick =
    Button.primary
        (Button.config
            |> Button.setDisabled disabled
            |> Button.setOnClick onClick
        )
        "save"


cancelButton onClick =
    Button.secondary
        (Button.config
            |> Button.setOnClick onClick
        )
        "cancel"
