module Page.Library.CqlPanel exposing (Model, Msg, init, update, view)

import Fhir.Attachment exposing (Attachment)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Material.Button exposing (buttonConfig, outlinedButton, unelevatedButton)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.LayoutGrid exposing (layoutGridCell, span12)
import Material.TextArea exposing (textArea, textAreaConfig)
import Maybe.Extra as MaybeExtra



-- MODEL


type alias Model =
    { attachment : Maybe Attachment
    , edit : Bool
    }


init : Maybe Attachment -> Model
init attachment =
    { attachment = attachment
    , edit = False
    }



-- UPDATE


type Msg
    = ClickedEdit
    | EnteredData String
    | ClickedCancel


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedEdit ->
            { model
                | edit = True
                , attachment =
                    MaybeExtra.or model.attachment (Just newAttachment)
            }

        EnteredData data ->
            let
                setData attachment =
                    { attachment | data = Just data }
            in
            { model | attachment = Maybe.map setData model.attachment }

        ClickedCancel ->
            { model | edit = False }


newAttachment =
    { contentType = Just "text/cql"
    , data = Nothing
    }



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe Attachment -> msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onSave } { attachment, edit } =
    layoutGridCell
        [ class "cql-panel"
        , span12
        , classList [ ( "cql-panel--edit", edit ) ]
        ]
        [ div [ class "cql-panel__title" ]
            [ h2 [ class "mdc-typography--headline5" ]
                [ text "CQL" ]
            , editButton onMsg
            ]
        , pre [ class "cql-panel__data" ]
            [ if edit then
                textArea
                    { textAreaConfig
                        | value =
                            attachment
                                |> Maybe.andThen .data
                                |> Maybe.withDefault ""
                        , fullwidth = True
                        , rows = Just 20
                        , onInput = Just (EnteredData >> onMsg)
                    }

              else
                attachment |> Maybe.andThen .data |> Maybe.withDefault "" |> text
            ]
        , div [ class "cql-panel__action-buttons" ]
            [ unelevatedButton
                { buttonConfig
                    | onClick = Just (onSave attachment)
                }
                "Save"
            , outlinedButton
                { buttonConfig
                    | onClick = Just (onMsg ClickedCancel)
                }
                "Cancel"
            ]
        ]


editButton onMsg =
    iconButton
        { iconButtonConfig
            | onClick = Just (onMsg ClickedEdit)
            , additionalAttributes = [ class "cql-panel__edit-button" ]
        }
        "edit"
