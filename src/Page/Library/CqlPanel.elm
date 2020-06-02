module Page.Library.CqlPanel exposing (Model, Msg, init, update, view)

import Fhir.Attachment exposing (Attachment)
import Html exposing (Html, div, h2, pre, text)
import Html.Attributes exposing (class, classList)
import Material.Button as Button
import Material.IconButton as IconButton
import Material.LayoutGrid as LayoutGrid exposing (span12)
import Material.TextArea as TextArea
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
    LayoutGrid.cell
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
                TextArea.filled
                    (TextArea.config
                        |> TextArea.setValue (attachment |> Maybe.andThen .data)
                        |> TextArea.setFullwidth True
                        |> TextArea.setRows (Just 20)
                        |> TextArea.setOnInput (EnteredData >> onMsg)
                    )

              else
                attachment |> Maybe.andThen .data |> Maybe.withDefault "" |> text
            ]
        , div [ class "cql-panel__action-buttons" ]
            [ Button.unelevated
                (Button.config
                    |> Button.setOnClick (onSave attachment)
                )
                "Save"
            , Button.outlined
                (Button.config
                    |> Button.setOnClick (onMsg ClickedCancel)
                )
                "Cancel"
            ]
        ]


editButton onMsg =
    IconButton.iconButton
        (IconButton.config
            |> IconButton.setOnClick (onMsg ClickedEdit)
            |> IconButton.setAttributes [ class "cql-panel__edit-button" ]
        )
        "edit"
