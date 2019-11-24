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
import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Expression exposing (Expression)
import Fhir.Measure.Stratifier as Stratifier
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Material.Icon exposing (icon, iconConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.TextArea exposing (textArea, textAreaConfig)
import Material.TextField exposing (textField, textFieldConfig)
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
            [ icon
                { iconConfig
                    | additionalAttributes =
                        [ class "measure-stratifier-dialog-component__icon" ]
                }
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
    iconButton { iconButtonConfig | onClick = Just onClick } "add_box"


removeComponentButton onClick =
    iconButton { iconButtonConfig | onClick = Just onClick } "remove_circle"


codeField onSave onMsg code =
    textField
        { textFieldConfig
            | label = Just "Name"
            , value = Maybe.withDefault "" code
            , onInput = Just (EnteredCode >> onMsg)
            , required = True
            , valid =
                Maybe.map (String.isEmpty >> not) code
                    |> Maybe.withDefault False
            , additionalAttributes = MaybeExtra.toList (Maybe.map onEnter onSave)
        }


descriptionField onSave onMsg description =
    textArea
        { textAreaConfig
            | label = Just "Description"
            , value = Maybe.withDefault "" description
            , onInput = Just (EnteredDescription >> onMsg)
            , additionalAttributes = MaybeExtra.toList (Maybe.map onEnter onSave)
        }


criteriaField onSave onMsg expression =
    textField
        { textFieldConfig
            | label = Just "CQL Criteria"
            , value = Maybe.withDefault "" expression
            , onInput = Just (EnteredCriteria >> onMsg)
            , required = True
            , valid =
                Maybe.map (String.isEmpty >> not) expression
                    |> Maybe.withDefault False
            , additionalAttributes = MaybeExtra.toList (Maybe.map onEnter onSave)
        }
