module Page.Measure.StratifierDialog exposing
    ( Model
    , Msg
    , doClose
    , doOpen
    , init
    , update
    , view
    )

import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Expression exposing (Expression)
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Http as Http
import List.Extra exposing (getAt, updateAt)
import Material.Button exposing (buttonConfig, textButton)
import Material.Dialog exposing (dialog, dialogConfig)
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
        , stratifierIdx : Int
        }


init base onMsg onSave =
    Closed { base = base, onMsg = onMsg, onSave = onSave }



-- UPDATE


type Msg
    = Close
    | EnteredCode String
    | EnteredDescription String
    | EnteredCriteria String
    | ClickedSave
    | CompletedSave (Result Http.Error Measure)


doOpen : Id -> Measure -> Int -> Int -> Model msg -> Model msg
doOpen measureId measure groupIdx stratifierIdx model =
    case model of
        Closed { base, onMsg, onSave } ->
            Open
                { base = base
                , onMsg = onMsg
                , onSave = onSave
                , measureId = measureId
                , measure = Debug.log "measure" measure
                , groupIdx = groupIdx
                , stratifierIdx = stratifierIdx
                }

        Open { base, onMsg, onSave } ->
            Open
                { base = base
                , onMsg = onMsg
                , onSave = onSave
                , measureId = measureId
                , measure = measure
                , groupIdx = groupIdx
                , stratifierIdx = stratifierIdx
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

        EnteredCode code ->
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
            (updateStratifier
                (\stratifier ->
                    { stratifier
                        | code = Just { coding = [], text = Just s }
                    }
                )
            )
        )
        model


setDescription : String -> Model msg -> Model msg
setDescription s model =
    updateMeasure
        (updateGroup
            (updateStratifier
                (\stratifier -> { stratifier | description = Just s })
            )
        )
        model


setCriteria : String -> Model msg -> Model msg
setCriteria s model =
    updateMeasure
        (updateGroup
            (updateStratifier
                (updateCriteria
                    (\criteria -> { criteria | expression = Just s })
                )
            )
        )
        model


updateCriteria f stratifier =
    { stratifier | criteria = Maybe.map f stratifier.criteria }


updateStratifier f groupIdx group =
    { group | stratifier = updateAt groupIdx f group.stratifier }


updateGroup f groupIdx stratifierIdx measure =
    { measure | group = updateAt groupIdx (f stratifierIdx) measure.group }


updateMeasure : (Int -> Int -> Measure -> Measure) -> Model msg -> Model msg
updateMeasure f model =
    case model of
        Open ({ measure, groupIdx, stratifierIdx } as model_) ->
            Open { model_ | measure = f groupIdx stratifierIdx measure }

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
                (Measure.encode (Debug.log "measure" measure))

        Closed _ ->
            Cmd.none



-- VIEW


view : Model msg -> Html Msg
view model =
    let
        stratifier =
            case model of
                Open { measure, groupIdx, stratifierIdx } ->
                    measure.group
                        |> getAt groupIdx
                        |> Maybe.map .stratifier
                        |> Maybe.andThen (getAt stratifierIdx)

                Closed _ ->
                    Nothing
    in
    dialog
        { dialogConfig
            | open = isOpen model
            , onClose = Just Close
            , additionalAttributes = [ class "measure-stratifier-dialog" ]
        }
        { title = Just "Stratifier"
        , content =
            [ stratifier
                |> Maybe.andThen .code
                |> Maybe.andThen .text
                |> codeField
            , stratifier
                |> Maybe.andThen .description
                |> descriptionField
            , stratifier
                |> Maybe.andThen .criteria
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


codeField code =
    textField
        { textFieldConfig
            | label = Just "Type"
            , value = code
            , onInput = Just EnteredCode
        }


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
