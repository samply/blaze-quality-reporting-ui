module Page.Measure.PopulationDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Component.Button as Button
import Component.Dialog as Dialog
import Component.TextArea as TextArea
import Component.TextField as TextField
import Events exposing (onEnter)
import Fhir.CodeableConcept as CodeableConcept
import Fhir.Measure as Measure
import Html exposing (Html)
import Html.Attributes exposing (class)
import Maybe.Extra as MaybeExtra
import Util


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
    { population | criteria = Maybe.map f population.criteria }


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
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen (isOpen model)
            |> Dialog.setOnClose (onMsg ClickedClose)
        )
        { title = Just "Population"
        , content =
            [ codeField onSave_ onMsg code
            , population
                |> Maybe.andThen .description
                |> descriptionField onSave_ onMsg
            , population
                |> Maybe.andThen .criteria
                |> Maybe.andThen .expression
                |> criteriaField onSave_ onMsg
            ]
        , actions =
            [ Button.secondary
                (Button.config
                    |> Button.setOnClick (onMsg ClickedClose)
                )
                "Cancel"
            , Button.primary
                (Button.config
                    |> Util.liftMaybe Button.setOnClick onSave_
                    |> Button.setDisabled
                        (code
                            |> Maybe.map String.isEmpty
                            |> Maybe.withDefault True
                        )
                )
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
    Html.div [ class "mb-2" ]
        [ Html.div [ class "mb-1" ] [ Html.text "Type" ]
        , TextField.outlined
            (TextField.config
                |> TextField.setValue code
                |> TextField.setOnInput (EnteredCode >> onMsg)
                |> TextField.setRequired True
                |> TextField.setValid
                    (Maybe.map (String.isEmpty >> not) code
                        |> Maybe.withDefault False
                    )
                |> TextField.setDisabled True
                |> TextField.setAttributes
                    (onSave
                        |> Maybe.map onEnter
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                    )
            )
        ]


descriptionField onSave onMsg description =
    Html.div [ class "mb-2" ]
        [ Html.div [ class "mb-1" ] [ Html.text "Description" ]
        , TextArea.outlined
            (TextArea.config
                |> TextArea.setValue description
                |> TextArea.setOnInput (EnteredDescription >> onMsg)
                |> TextArea.setAttributes
                    (onSave
                        |> Maybe.map onEnter
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                    )
            )
        ]


criteriaField onSave onMsg expression =
    Html.div [ class "mb-2" ]
        [ Html.div [ class "mb-1" ] [ Html.text "Criteria" ]
        , TextField.outlined
            (TextField.config
                |> TextField.setValue expression
                |> TextField.setOnInput (EnteredCriteria >> onMsg)
                |> TextField.setAttributes
                    (onSave
                        |> Maybe.map onEnter
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                    )
            )
        ]
