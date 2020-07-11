module Component.Sidebar.SubjectPanel exposing (Model, Msg, init, update, view)

{-| This component is the subject input box in the sidebar.
-}

import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Events
import Html exposing (text)
import Material.Button as Button
import Material.TextField as TextField



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
    , valid = True
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
            ( { model
                | enteredSubject = Just model.originalSubject
                , edit = False
              }
            , Cmd.none
            )

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
                        |> TextField.setValue enteredSubject
                        |> TextField.setRequired True
                        |> TextField.setValid valid
                        |> TextField.setOnInput (EnteredSubject >> onMsg)
                        |> TextField.setAttributes
                            [ if valid then
                                Events.onEnterEsc
                                    (onSave enteredSubject)
                                    (onMsg ClickedCancel)

                              else
                                Events.onEsc (onMsg ClickedCancel)
                            ]
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
    Button.unelevated
        (Button.config
            |> Button.setDense True
            |> Button.setDisabled disabled
            |> Button.setOnClick onClick
        )
        "save"


cancelButton onClick =
    Button.outlined
        (Button.config
            |> Button.setDense True
            |> Button.setOnClick onClick
        )
        "cancel"
