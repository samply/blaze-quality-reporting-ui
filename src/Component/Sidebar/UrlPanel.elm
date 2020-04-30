module Component.Sidebar.UrlPanel exposing (Model, Msg, init, update, view)

{-| This component is a canonical URL input box in the sidebar.

It can be used for Resources like Library or Measure which have canonical URLs.

The component has a read-only view with an edit button which transforms the view
into an editable view with save an cancel actions. The message constructor for
the onSave action can be supplied to the ['view'](#view) function.

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
import Events
import Fhir.PrimitiveTypes exposing (Uri)
import Html exposing (text)
import Material.Button exposing (buttonConfig, outlinedButton, unelevatedButton)
import Material.TextField exposing (textField, textFieldConfig)



-- MODEL


type alias Model =
    { originalUrl : Maybe Uri
    , enteredUrl : Maybe Uri
    , edit : Bool
    }


init : Maybe Uri -> Model
init url =
    { originalUrl = url
    , enteredUrl = url
    , edit = False
    }



-- UPDATE


type Msg
    = ClickedEdit
    | ClickedCancel
    | EnteredUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedEdit ->
            ( { model | edit = True }, Cmd.none )

        ClickedCancel ->
            ( { model | enteredUrl = model.originalUrl, edit = False }
            , Cmd.none
            )

        EnteredUrl s ->
            ( { model | enteredUrl = Just s }, Cmd.none )



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe Uri -> msg
    }


view : Config msg -> Model -> SidebarEntry msg
view { onMsg, onSave } { enteredUrl, edit } =
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
                        | value = enteredUrl |> Maybe.withDefault ""
                        , onInput = Just (EnteredUrl >> onMsg)
                        , outlined = True
                        , additionalAttributes =
                            [ Events.onEnterEsc
                                (onSave enteredUrl)
                                (onMsg ClickedCancel)
                            ]
                    }
                ]

            else
                [ enteredUrl |> Maybe.withDefault "<not-specified>" |> text ]
        , sidebarEntryActionButtons [] <|
            if edit then
                [ unelevatedButton
                    { buttonConfig | onClick = Just (onSave enteredUrl) }
                    "save"
                , outlinedButton
                    { buttonConfig | onClick = Just (onMsg ClickedCancel) }
                    "cancel"
                ]

            else
                []
        ]
