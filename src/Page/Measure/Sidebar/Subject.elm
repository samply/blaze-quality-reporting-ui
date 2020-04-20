module Page.Measure.Sidebar.Subject exposing (Model, Msg, init, update, view)

{-| This component is the subject input box in the sidebar.
-}

import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Events exposing (onEnterEsc)
import Html exposing (text)
import Material.Button as Button
import Material.TextField as TextField



-- MODEL


type alias Model =
    { originalSubject : String
    , enteredSubject : Maybe String
    , edit : Bool
    }


init : String -> Model
init subject =
    { originalSubject = subject
    , enteredSubject = Just subject
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
            ( { model
                | enteredSubject = Just model.originalSubject
                , edit = False
              }
            , Cmd.none
            )

        EnteredSubject subject ->
            ( { model | enteredSubject = Just subject }, Cmd.none )



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe String -> msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { onMsg, onSave } { originalSubject, enteredSubject, edit } =
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
                        |> TextField.setOnInput (EnteredSubject >> onMsg)
                        |> TextField.setAttributes
                            [ onEnterEsc
                                (onSave enteredSubject)
                                (onMsg ClickedCancel)
                            ]
                    )
                ]

            else
                [ text originalSubject ]
        , SidebarEntry.actionButtons [] <|
            if edit then
                [ Button.unelevated
                    (Button.config |> Button.setOnClick (onSave enteredSubject))
                    "save"
                , Button.outlined
                    (Button.config |> Button.setOnClick (onMsg ClickedCancel))
                    "cancel"
                ]

            else
                []
        ]
