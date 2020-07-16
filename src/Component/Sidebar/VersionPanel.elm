module Component.Sidebar.VersionPanel exposing (Model, Msg, init, update, view)

{-| This component is a version input box in the sidebar.

It can be used for Resources like Library or Measure which have versions.

The component has a read-only view with an edit button which transforms the view
into an editable view with save an cancel actions. The message constructor for
the onSave action can be supplied to the ['view'](#view) function.

-}

import Browser.Dom as Dom
import Component.Button as Button
import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Component.TextField as TextField
import Html exposing (text)
import Html.Attributes exposing (class)
import Task



-- MODEL


type alias Model =
    { originalVersion : Maybe String
    , enteredVersion : Maybe String
    , edit : Bool
    }


init : Maybe String -> Model
init version =
    { originalVersion = version
    , enteredVersion = version
    , edit = False
    }



-- UPDATE


type Msg
    = ClickedEdit
    | ClickedCancel
    | TextFieldFocused (Result Dom.Error ())
    | EnteredVersion String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedEdit ->
            ( { model | edit = True }
            , Task.attempt TextFieldFocused (Dom.focus "version")
            )

        TextFieldFocused _ ->
            ( model, Cmd.none )

        ClickedCancel ->
            ( { model | enteredVersion = model.originalVersion, edit = False }
            , Cmd.none
            )

        EnteredVersion s ->
            ( { model | enteredVersion = emptyToNothing s }, Cmd.none )


emptyToNothing : String -> Maybe String
emptyToNothing s =
    if String.isEmpty s then
        Nothing

    else
        Just s



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe String -> msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { onMsg, onSave } { originalVersion, enteredVersion, edit } =
    SidebarEntry.view
        SidebarEntry.config
        [ SidebarEntry.title []
            [ text "Version"
            , SidebarEntry.editButton
                (Button.config |> Button.setOnClick (onMsg ClickedEdit))
            ]
        , SidebarEntry.content [] <|
            if edit then
                [ TextField.outlined
                    (TextField.config
                        |> TextField.setId (Just "version")
                        |> TextField.setValue enteredVersion
                        |> TextField.setOnInput (EnteredVersion >> onMsg)
                        |> TextField.setOnEnter (onSave enteredVersion)
                        |> TextField.setOnEsc (onMsg ClickedCancel)
                    )
                ]

            else
                [ originalVersion |> Maybe.withDefault "<not-specified>" |> text ]
        , SidebarEntry.actionButtons [] <|
            if edit then
                [ saveButton (onSave enteredVersion)
                , cancelButton (onMsg ClickedCancel)
                ]

            else
                []
        ]


saveButton onClick =
    Button.primary
        (Button.config
            |> Button.setOnClick onClick
        )
        "Save"


cancelButton onClick =
    Button.secondary
        (Button.config
            |> Button.setOnClick onClick
        )
        "Cancel"
