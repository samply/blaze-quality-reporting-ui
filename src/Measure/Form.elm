module Measure.Form exposing (Model, Msg, init, update, view)

import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Coding exposing (Coding)
import Fhir.Expression exposing (Expression)
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Http
import Material.Button exposing (buttonConfig, unelevatedButton)
import Material.Select
    exposing
        ( filledSelect
        , selectConfig
        , selectOption
        , selectOptionConfig
        )
import Material.TextField exposing (textField, textFieldConfig)



-- MODEL


type alias Model =
    { base : String
    , measure : Measure
    }


init : String -> Measure -> Model
init base measure =
    { base = base
    , measure = measure
    }



-- UPDATE


type Msg
    = EnteredTitle String
    | SelectedSubject String
    | EnteredCriteria String
    | ClickedSave
    | CompletedSave (Result Http.Error Measure)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredTitle title ->
            ( updateMeasure (\measure -> { measure | title = Just title }) model
            , Cmd.none
            )

        SelectedSubject subjectCode ->
            ( updateMeasure
                (updateSubject
                    (updateCoding 0
                        (\coding -> { coding | code = Just subjectCode })
                    )
                )
                model
            , Cmd.none
            )

        EnteredCriteria criteria ->
            ( updateMeasure
                (updateGroup 0
                    (updatePopulation 0
                        (updateCriteria
                            (\expression ->
                                { expression | expression = Just criteria }
                            )
                        )
                    )
                )
                model
            , Cmd.none
            )

        ClickedSave ->
            ( model, saveMeasure model )

        CompletedSave (Ok measure) ->
            ( { model | measure = measure }, Cmd.none )

        CompletedSave (Err _) ->
            ( model, Cmd.none )


updateMeasure : (Measure -> Measure) -> Model -> Model
updateMeasure f model =
    { model | measure = f model.measure }


updateSubject : (CodeableConcept -> CodeableConcept) -> Measure -> Measure
updateSubject f model =
    { model | subject = Maybe.map f model.subject }


updateCoding : Int -> (Coding -> Coding) -> CodeableConcept -> CodeableConcept
updateCoding idx f codeableConcept =
    { codeableConcept
        | coding = List.indexedMap (updateAt idx f) codeableConcept.coding
    }


updateGroup : Int -> (Measure.Group -> Measure.Group) -> Measure -> Measure
updateGroup idx f measure =
    { measure | group = List.indexedMap (updateAt idx f) measure.group }


updatePopulation :
    Int
    -> (Measure.Population -> Measure.Population)
    -> Measure.Group
    -> Measure.Group
updatePopulation idx f group =
    { group | population = List.indexedMap (updateAt idx f) group.population }


updateCriteria :
    (Expression -> Expression)
    -> Measure.Population
    -> Measure.Population
updateCriteria f model =
    { model | criteria = f model.criteria }


updateAt idx f =
    \i x ->
        if idx == i then
            f x

        else
            x


saveMeasure { base, measure } =
    let
        json =
            Measure.encode measure
    in
    case measure.id of
        Just id ->
            FhirHttp.update CompletedSave base "Measure" id Measure.decoder json

        Nothing ->
            FhirHttp.create CompletedSave base "Measure" Measure.decoder json



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onClose : msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onClose } { measure } =
    div []
        [ p []
            [ titleField measure.title |> Html.map onMsg
            , subjectSelector measure.subject |> Html.map onMsg
            ]
        , div [] <|
            List.map (groupPanel >> Html.map onMsg) measure.group
        , p [] [ saveButton measure |> Html.map onMsg, closeButton onClose ]
        ]


titleField title =
    textField
        { textFieldConfig
            | label = Just "Title"
            , value = title
            , onInput = Just EnteredTitle
        }


subjectSelector : Maybe CodeableConcept -> Html Msg
subjectSelector subject =
    filledSelect
        { selectConfig
            | label = "Subject"
            , value =
                subject
                    |> Maybe.map .coding
                    |> Maybe.andThen List.head
                    |> Maybe.andThen .code
            , onChange = Just SelectedSubject
            , additionalAttributes = [ style "margin-left" "1em" ]
        }
        [ selectOption
            { selectOptionConfig | value = "Patient" }
            [ text "Patient" ]
        , selectOption
            { selectOptionConfig | value = "Specimen" }
            [ text "Specimen" ]
        ]


groupPanel { population } =
    div [] <|
        List.map populationPanel population


populationPanel { criteria } =
    criteriaField criteria.expression


criteriaField criteria =
    textField
        { textFieldConfig
            | label = Just "Criteria"
            , value = criteria
            , onInput = Just EnteredCriteria
        }


saveButton { id } =
    unelevatedButton
        { buttonConfig | onClick = Just ClickedSave }
        (case id of
            Just _ ->
                "Update"

            Nothing ->
                "Save"
        )


closeButton onClose =
    unelevatedButton
        { buttonConfig
            | onClick = Just onClose
            , additionalAttributes = [ style "margin-left" "1em" ]
        }
        "Close"
