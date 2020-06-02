module Component.Header exposing (Model, Msg, init, update, view)

import Events
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Material.Button as Button
import Material.IconButton as IconButton
import Material.LayoutGrid as LayoutGrid exposing (span12)
import Material.TextArea as TextArea
import Material.TextField as TextField



-- MODEL


type alias Model =
    { title : Maybe String
    , enteredTitle : Maybe String
    , description : Maybe String
    , enteredDescription : Maybe String
    , edit : Bool
    }


init : Maybe String -> Maybe String -> Model
init title description =
    { title = title
    , enteredTitle = title
    , description = description
    , enteredDescription = description
    , edit = False
    }



-- UPDATE


type Msg
    = ClickedEdit
    | EnteredTitle String
    | EnteredDescription String
    | ClickedCancel


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedEdit ->
            { model | edit = True }

        EnteredTitle title ->
            { model | enteredTitle = blankToNothing title }

        EnteredDescription description ->
            { model | enteredDescription = blankToNothing description }

        ClickedCancel ->
            { model
                | enteredTitle = model.title
                , enteredDescription = model.description
                , edit = False
            }


blankToNothing s =
    if String.isEmpty s then
        Nothing

    else
        Just s



-- VIEW


type alias Config msg =
    { onSave : Maybe String -> Maybe String -> msg
    , onDelete : msg
    , onMsg : Msg -> msg
    }


view : Config msg -> Model -> Html msg
view { onSave, onDelete, onMsg } { title, enteredTitle, description, enteredDescription, edit } =
    LayoutGrid.cell
        [ class "measure-header"
        , span12
        , classList [ ( "measure-header--edit", edit ) ]
        ]
    <|
        [ div [ class "measure-header__title" ]
            (if edit then
                [ TextField.outlined
                    (TextField.config
                        |> TextField.setValue enteredTitle
                        |> TextField.setOnInput (EnteredTitle >> onMsg)
                        |> TextField.setAttributes
                            [ Events.onEnterEsc
                                (onSave enteredTitle enteredDescription)
                                (onMsg ClickedCancel)
                            ]
                    )
                ]

             else
                [ h2 [ class "mdc-typography--headline5" ]
                    [ text (Maybe.withDefault "<not specified>" title) ]
                , IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setOnClick (onMsg ClickedEdit)
                    )
                    "edit"
                ]
            )
        ]
            ++ (if edit then
                    [ div [ class "measure-header__description" ]
                        [ TextArea.outlined
                            (TextArea.config
                                |> TextArea.setFullwidth True
                                |> TextArea.setOnInput (EnteredDescription >> onMsg)
                            )
                        ]
                    , div [ class "measure-header__action-buttons" ]
                        [ saveButton (onSave enteredTitle enteredDescription)
                        , div [ class "measure-header__action-buttons-right" ]
                            [ deleteButton onDelete
                            , cancelButton (onMsg ClickedCancel)
                            ]
                        ]
                    ]

                else
                    case description of
                        Just s ->
                            [ div [ class "measure-header__description" ]
                                [ text s ]
                            ]

                        Nothing ->
                            []
               )


saveButton onClick =
    Button.unelevated
        (Button.config
            |> Button.setOnClick onClick
        )
        "Save"


deleteButton onClick =
    Button.unelevated
        (Button.config
            |> Button.setOnClick onClick
            |> Button.setAttributes
                [ class "measure-header__delete-button" ]
        )
        "Delete"


cancelButton onClick =
    Button.outlined
        (Button.config
            |> Button.setOnClick onClick
        )
        "Cancel"
