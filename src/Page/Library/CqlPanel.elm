module Page.Library.CqlPanel exposing (Model, Msg, init, update, view)

import Component.Button as Button
import Component.TextArea as TextArea
import Fhir.Attachment exposing (Attachment)
import Html exposing (Html, div, h2, pre, text)
import Html.Attributes exposing (class, classList)
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
    Html.div [ class "" ]
        [ Html.div [ class "flex justify-between mb-4" ]
            [ Html.h2 [ class "text-lg" ]
                [ text "CQL" ]
            , editButton onMsg
            ]
        , if edit then
            div []
                [ TextArea.outlined
                    (TextArea.config
                        |> TextArea.setValue (attachment |> Maybe.andThen .data)
                        |> TextArea.setRows (Just 20)
                        |> TextArea.setOnInput (EnteredData >> onMsg)
                    )
                , div [ class "text-right space-x-2" ]
                    [ Button.secondary
                        (Button.config
                            |> Button.setOnClick (onMsg ClickedCancel)
                        )
                        "Cancel"
                    , Button.primary
                        (Button.config
                            |> Button.setOnClick (onSave attachment)
                        )
                        "Save"
                    ]
                ]

          else
            pre [ class "text-sm" ]
                [ attachment |> Maybe.andThen .data |> Maybe.withDefault "" |> text ]
        ]


editButton onMsg =
    Button.secondary
        (Button.config
            |> Button.setOnClick (onMsg ClickedEdit)
        )
        "Edit"
