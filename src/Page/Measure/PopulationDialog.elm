module Page.Measure.PopulationDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure, measurePopulationType)
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Http as Http
import List.Extra exposing (getAt, updateAt)
import Material.Button exposing (buttonConfig, textButton)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.Select
    exposing
        ( filledSelect
        , selectConfig
        , selectOption
        , selectOptionConfig
        )
import Material.TextArea exposing (textArea, textAreaConfig)
import Material.TextField exposing (textField, textFieldConfig)
import Task


type Model msg
    = Closed
        { base : String
        , onMsg : Msg -> msg
        , onSave : Measure -> msg
        }
    | Open
        { base : String
        , onMsg : Msg -> msg
        , onSave : Measure -> msg
        , measureId : Id
        , measure : Measure
        , groupIdx : Int
        , populationIdx : Int
        }


init base onMsg onSave =
    Closed { base = base, onMsg = onMsg, onSave = onSave }



-- UPDATE


type Msg
    = Close
    | SelectedCode String
    | EnteredDescription String
    | EnteredCriteria String
    | ClickedSave
    | CompletedSave (Result Http.Error Measure)


doOpen : Id -> Measure -> Int -> Int -> Model msg -> Model msg
doOpen measureId measure groupIdx populationIdx model =
    case model of
        Closed { base, onMsg, onSave } ->
            Open
                { base = base
                , onMsg = onMsg
                , onSave = onSave
                , measureId = measureId
                , measure = measure
                , groupIdx = groupIdx
                , populationIdx = populationIdx
                }

        Open { base, onMsg, onSave } ->
            Open
                { base = base
                , onMsg = onMsg
                , onSave = onSave
                , measureId = measureId
                , measure = measure
                , groupIdx = groupIdx
                , populationIdx = populationIdx
                }


doClose : Model msg -> Model msg
doClose model =
    case model of
        Open { base, onMsg, onSave } ->
            Closed
                { base = base
                , onMsg = onMsg
                , onSave = onSave
                }

        Closed _ ->
            model


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    let
        ( onMsg, onSave ) =
            case model of
                Open model_ ->
                    ( model_.onMsg, model_.onSave )

                Closed model_ ->
                    ( model_.onMsg, model_.onSave )
    in
    case msg of
        Close ->
            ( doClose model, Cmd.none )

        SelectedCode code ->
            ( setCode code model, Cmd.none )

        EnteredDescription description ->
            ( setDescription description model, Cmd.none )

        EnteredCriteria criteria ->
            ( setCriteria criteria model, Cmd.none )

        ClickedSave ->
            ( model, Cmd.map onMsg (saveMeasure model) )

        CompletedSave (Ok measure) ->
            ( doClose model
            , Task.succeed measure |> Task.perform onSave
            )

        CompletedSave (Err _) ->
            ( model, Cmd.none )


setCode : String -> Model msg -> Model msg
setCode s model =
    updateMeasure
        (updateGroup
            (updatePopulation
                (\population ->
                    { population
                        | code = Just { coding = [ measurePopulationType s ] }
                    }
                )
            )
        )
        model


setDescription : String -> Model msg -> Model msg
setDescription s model =
    updateMeasure
        (updateGroup
            (updatePopulation
                (\population -> { population | description = Just s })
            )
        )
        model


setCriteria : String -> Model msg -> Model msg
setCriteria s model =
    updateMeasure
        (updateGroup
            (updatePopulation
                (updateCriteria
                    (\criteria -> { criteria | expression = Just s })
                )
            )
        )
        model


updateCriteria f population =
    { population | criteria = f population.criteria }


updatePopulation f groupIdx group =
    { group | population = updateAt groupIdx f group.population }


updateGroup f groupIdx populationIdx measure =
    { measure | group = updateAt groupIdx (f populationIdx) measure.group }


updateMeasure : (Int -> Int -> Measure -> Measure) -> Model msg -> Model msg
updateMeasure f model =
    case model of
        Open ({ measure, groupIdx, populationIdx } as model_) ->
            Open { model_ | measure = f groupIdx populationIdx measure }

        Closed _ ->
            model


saveMeasure model =
    case model of
        Open { base, measureId, measure } ->
            FhirHttp.update CompletedSave
                base
                "Measure"
                measureId
                Measure.decoder
                (Measure.encode measure)

        Closed _ ->
            Cmd.none



-- VIEW


view : Model msg -> Html Msg
view model =
    let
        population =
            case model of
                Open { measure, groupIdx, populationIdx } ->
                    measure.group
                        |> getAt groupIdx
                        |> Maybe.map .population
                        |> Maybe.andThen (getAt populationIdx)

                Closed _ ->
                    Nothing
    in
    dialog
        { dialogConfig
            | open = isOpen model
            , onClose = Just Close
            , additionalAttributes = [ class "measure-population-dialog" ]
        }
        { title = Just "Population"
        , content =
            [ population
                |> Maybe.andThen .code
                |> codeSelector
            , population
                |> Maybe.andThen .description
                |> descriptionField
            , population
                |> Maybe.map .criteria
                |> Maybe.andThen .expression
                |> criteriaField
            ]
        , actions =
            [ textButton
                { buttonConfig
                    | onClick = Just Close
                }
                "Cancel"
            , textButton
                { buttonConfig
                    | onClick = Just ClickedSave
                }
                "Save"
            ]
        }


isOpen model =
    case model of
        Open _ ->
            True

        Closed _ ->
            False


codeSelector : Maybe CodeableConcept -> Html Msg
codeSelector code =
    filledSelect
        { selectConfig
            | label = "Type"
            , value =
                code
                    |> Maybe.map .coding
                    |> Maybe.andThen List.head
                    |> Maybe.andThen .code
            , onChange = Just SelectedCode
        }
        (List.map option
            [ "initial-population"
            , "numerator"
            , "numerator-exclusion"
            , "denominator"
            , "denominator-exclusion"
            , "denominator-exception"
            , "measure-population"
            , "measure-population-exclusion"
            , "measure-observation"
            ]
        )


option code =
    selectOption
        { selectOptionConfig | value = code }
        [ text code ]


descriptionField description =
    textArea
        { textAreaConfig
            | label = Just "Description"
            , value = description
            , onInput = Just EnteredDescription
        }


criteriaField criteria =
    textField
        { textFieldConfig
            | label = Just "Criteria"
            , value = criteria
            , onInput = Just EnteredCriteria
        }
