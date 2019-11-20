module Page.Measure.PopulationDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Fhir.CodeableConcept as CodeableConcept exposing (CodeableConcept)
import Fhir.Expression exposing (Expression)
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)
import Html.Events exposing (keyCode, on)
import Json.Decode as Decode
import Material.Button exposing (buttonConfig, textButton)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.TextArea exposing (textArea, textAreaConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Maybe.Extra as MaybeExtra


type Model
    = Closed
    | Open { population : Measure.Population }


init : Model
init =
    Closed



-- UPDATE


type Msg
    = ClickedClose
    | EnteredCode String
    | EnteredDescription String
    | EnteredCriteria String


doOpen : Measure.Population -> Model -> Model
doOpen population _ =
    Open { population = setTypeToInitialPopulation population }


{-| Sets the type of the population to `initial-population` because we don't
support other types.
-}
setTypeToInitialPopulation population =
    { population
        | code =
            Just
                (CodeableConcept.ofOneCoding
                    (Measure.populationType "initial-population")
                )
    }


doClose : Model -> Model
doClose _ =
    Closed


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedClose ->
            doClose model

        EnteredCode code ->
            updatePopulation
                (\population ->
                    { population
                        | code = Just { coding = [], text = Just code }
                    }
                )
                model

        EnteredDescription description ->
            updatePopulation
                (\population -> { population | description = Just description })
                model

        EnteredCriteria expression ->
            updatePopulation
                (updateCriteria
                    (\criteria -> { criteria | expression = Just expression })
                )
                model


updateCriteria f population =
    { population | criteria = f population.criteria }


updatePopulation f model =
    case model of
        Open data ->
            Open { data | population = f data.population }

        Closed ->
            model



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe (Measure.Population -> msg)
    }


view : Config msg -> Model -> Html msg
view { onMsg, onSave } model =
    let
        population =
            case model of
                Open data ->
                    Just data.population

                Closed ->
                    Nothing

        code =
            population
                |> Maybe.andThen .code
                |> Maybe.andThen (.coding >> List.head)
                |> Maybe.andThen .code

        onSave_ =
            MaybeExtra.andMap population onSave
    in
    dialog
        { dialogConfig
            | open = isOpen model
            , onClose = Just (onMsg ClickedClose)
            , additionalAttributes = [ class "measure-population-dialog" ]
        }
        { title = Just "Population"
        , content =
            [ codeField onSave_ onMsg code
            , population
                |> Maybe.andThen .description
                |> descriptionField onSave_ onMsg
            , population
                |> Maybe.map .criteria
                |> Maybe.andThen .expression
                |> criteriaField onSave_ onMsg
            ]
        , actions =
            [ textButton
                { buttonConfig
                    | onClick = Just (onMsg ClickedClose)
                }
                "Cancel"
            , textButton
                { buttonConfig
                    | onClick = onSave_
                    , disabled =
                        code
                            |> Maybe.map String.isEmpty
                            |> Maybe.withDefault True
                }
                "Save"
            ]
        }


isOpen model =
    case model of
        Open _ ->
            True

        Closed ->
            False


codeField onSave onMsg code =
    textField
        { textFieldConfig
            | label = Just "Type"
            , value = Maybe.withDefault "" code
            , onInput = Just (EnteredCode >> onMsg)
            , required = True
            , valid =
                Maybe.map (String.isEmpty >> not) code
                    |> Maybe.withDefault False
            , disabled = True
            , additionalAttributes =
                onSave
                    |> Maybe.map onEnter
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
        }


descriptionField onSave onMsg description =
    textArea
        { textAreaConfig
            | label = Just "Description"
            , value = Maybe.withDefault "" description
            , onInput = Just (EnteredDescription >> onMsg)
            , additionalAttributes =
                onSave
                    |> Maybe.map onEnter
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
        }


criteriaField onSave onMsg expression =
    textField
        { textFieldConfig
            | label = Just "Criteria"
            , value = Maybe.withDefault "" expression
            , onInput = Just (EnteredCriteria >> onMsg)
            , additionalAttributes =
                onSave
                    |> Maybe.map onEnter
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
        }


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keyup" (Decode.andThen isEnter keyCode)
