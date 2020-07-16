module Component.Header exposing (Model, Msg, init, update, view)

import Component.Button as Button
import Component.TextArea as TextArea
import Component.TextField as TextField
import Events
import Html exposing (..)
import Html.Attributes exposing (class)



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
    div [ class "mb-4" ]
        ([ div [ class "flex justify-between mb-4" ]
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
                [ h2 [ class "text-xl leading-loose" ]
                    [ text (Maybe.withDefault "<not specified>" title) ]
                , Button.secondary
                    (Button.config
                        |> Button.setOnClick (onMsg ClickedEdit)
                        |> Button.setAttributes [ class "self-start" ]
                    )
                    "Edit"
                ]
            )
         ]
            ++ (if edit then
                    [ div [ class "" ]
                        [ TextArea.outlined
                            (TextArea.config
                                |> TextArea.setValue enteredDescription
                                |> TextArea.setRows (Just 6)
                                |> TextArea.setOnInput (EnteredDescription >> onMsg)
                            )
                        ]
                    , div [ class "flex justify-between" ]
                        [ saveButton (onSave enteredTitle enteredDescription)
                        , div [ class "space-x-2" ]
                            [ deleteButton onDelete
                            , cancelButton (onMsg ClickedCancel)
                            ]
                        ]
                    ]

                else
                    case description of
                        Just s ->
                            [ div [ class "" ]
                                [ text s ]
                            ]

                        Nothing ->
                            []
               )
        )


saveButton onClick =
    Button.primary
        (Button.config
            |> Button.setOnClick onClick
        )
        "Save"


deleteButton onClick =
    Button.secondary
        (Button.config
            |> Button.setOnClick onClick
            |> Button.setAttributes
                [ class "measure-header__delete-button" ]
        )
        "Delete"


cancelButton onClick =
    Button.secondary
        (Button.config
            |> Button.setOnClick onClick
        )
        "Cancel"
