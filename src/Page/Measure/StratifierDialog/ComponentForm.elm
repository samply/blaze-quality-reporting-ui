module Page.Measure.StratifierDialog.ComponentForm exposing
    ( Control(..)
    , Model
    , Msg
    , init
    , isValid
    , toStratifierComponent
    , update
    , view
    )

import Events exposing (onEnter)
import Fhir.Measure.Stratifier as Stratifier
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Material.Icon as Icon
import Material.IconButton as IconButton
import Material.TextArea as TextArea
import Material.TextField as TextField
import Maybe.Extra as MaybeExtra



-- MODEL


type alias Model =
    Stratifier.Component


init : Stratifier.Component -> Model
init component =
    component


toStratifierComponent : Model -> Stratifier.Component
toStratifierComponent model =
    model


isValid : Model -> Bool
isValid model =
    (model.code |> Maybe.andThen .text |> MaybeExtra.isJust)
        && (model.criteria.expression |> MaybeExtra.isJust)



-- UPDATE


type Msg
    = EnteredCode String
    | EnteredDescription String
    | EnteredCriteria String


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredCode code ->
            { model | code = Just { coding = [], text = Just code } }

        EnteredDescription description ->
            { model | description = Just description }

        EnteredCriteria expression ->
            updateCriteria
                (\criteria -> { criteria | expression = Just expression })
                model


updateCriteria f model =
    { model | criteria = f model.criteria }



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , control : Control msg
    , onSave : Maybe msg
    }


type Control msg
    = AddComponent msg
    | RemoveComponent msg


view : Config msg -> Model -> Html msg
view { onMsg, control, onSave } model =
    div [ class "measure-stratifier-dialog-component" ]
        [ div [ class "measure-stratifier-dialog-component__front" ]
            [ Icon.icon
                [ class "measure-stratifier-dialog-component__icon" ]
                "filter_1"
            , case control of
                AddComponent msg ->
                    addComponentButton msg

                RemoveComponent msg ->
                    removeComponentButton msg
            ]
        , div [ class "measure-stratifier-dialog-component__form" ]
            [ codeField onSave onMsg (Maybe.andThen .text model.code)
            , descriptionField onSave onMsg model.description
            , criteriaField onSave onMsg model.criteria.expression
            ]
        ]


addComponentButton onClick =
    IconButton.iconButton
        (IconButton.config |> IconButton.setOnClick onClick)
        "add_box"


removeComponentButton onClick =
    IconButton.iconButton
        (IconButton.config |> IconButton.setOnClick onClick)
        "remove_circle"


codeField onSave onMsg code =
    TextField.filled
        (TextField.config
            |> TextField.setLabel (Just "Name")
            |> TextField.setValue code
            |> TextField.setOnInput (EnteredCode >> onMsg)
            |> TextField.setRequired True
            |> TextField.setValid
                (Maybe.map (String.isEmpty >> not) code
                    |> Maybe.withDefault False
                )
            |> TextField.setAttributes (MaybeExtra.toList (Maybe.map onEnter onSave))
        )


descriptionField onSave onMsg description =
    TextArea.filled
        (TextArea.config
            |> TextArea.setLabel (Just "Description")
            |> TextArea.setValue description
            |> TextArea.setOnInput (EnteredDescription >> onMsg)
            |> TextArea.setAttributes (MaybeExtra.toList (Maybe.map onEnter onSave))
        )


criteriaField onSave onMsg expression =
    TextField.filled
        (TextField.config
            |> TextField.setLabel (Just "CQL Criteria Name")
            |> TextField.setValue expression
            |> TextField.setOnInput (EnteredCriteria >> onMsg)
            |> TextField.setRequired True
            |> TextField.setValid
                (Maybe.map (String.isEmpty >> not) expression
                    |> Maybe.withDefault False
                )
            |> TextField.setAttributes (MaybeExtra.toList (Maybe.map onEnter onSave))
        )
