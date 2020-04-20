module Component.Sidebar.UrlPanel exposing (Model, Msg, init, update, view)

{-| This component is a canonical URL input box in the sidebar.

It can be used for Resources like Library or Measure which have canonical URLs.

The component has a read-only view with an edit button which transforms the view
into an editable view with save an cancel actions. The message constructor for
the onSave action can be supplied to the ['view'](#view) function.

-}

import Component.Sidebar.Entry as SidebarEntry exposing (SidebarEntry)
import Events
import Fhir.PrimitiveTypes exposing (Uri)
import Html exposing (text)
import Html.Attributes exposing (class)
import Material.Button as Button
import Material.TextField as TextField



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
view { onMsg, onSave } { originalUrl, enteredUrl, edit } =
    SidebarEntry.view
        (SidebarEntry.config |> SidebarEntry.setAttributes [ class "url-panel" ])
        [ SidebarEntry.title []
            [ text "URL"
            , SidebarEntry.editButton
                (Button.config |> Button.setOnClick (onMsg ClickedEdit))
            ]
        , SidebarEntry.content [] <|
            if edit then
                [ TextField.outlined
                    (TextField.config
                        |> TextField.setValue enteredUrl
                        |> TextField.setOnInput (EnteredUrl >> onMsg)
                        |> TextField.setAttributes
                            [ Events.onEnterEsc
                                (onSave enteredUrl)
                                (onMsg ClickedCancel)
                            ]
                    )
                ]

            else
                [ originalUrl |> Maybe.withDefault "<not-specified>" |> text ]
        , SidebarEntry.actionButtons [] <|
            if edit then
                [ Button.unelevated
                    (Button.config |> Button.setOnClick (onSave enteredUrl))
                    "save"
                , Button.outlined
                    (Button.config |> Button.setOnClick (onMsg ClickedCancel))
                    "cancel"
                ]

            else
                []
        ]
