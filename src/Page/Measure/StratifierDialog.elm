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
    | Open { stratifier : Measure.Stratifier }


init : Model
init =
    Closed



-- UPDATE


type Msg
    = ClickedClose
    | EnteredCode String
    | EnteredDescription String
    | EnteredCriteria String


doOpen : Measure.Stratifier -> Model -> Model
doOpen stratifier _ =
    Open { stratifier = stratifier }


doClose : Model -> Model
doClose _ =
    Closed


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedClose ->
            doClose model

        EnteredCode code ->
            updateStratifier
                (\stratifier ->
                    { stratifier
                        | code = Just { coding = [], text = Just code }
                    }
                )
                model

        EnteredDescription description ->
            updateStratifier
                (\stratifier -> { stratifier | description = Just description })
                model

        EnteredCriteria expression ->
            updateStratifier
                (updateCriteria
                    (\criteria -> { criteria | expression = Just expression })
                )
                model


updateCriteria f stratifier =
    { stratifier | criteria = Maybe.map f stratifier.criteria }


updateStratifier f model =
    case model of
        Open data ->
            Open { data | stratifier = f data.stratifier }

        Closed ->
            model



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Maybe (Measure.Stratifier -> msg)
    }


view : Config msg -> Model -> Html msg
view { onMsg, onSave } model =
    let
        stratifier =
            case model of
                Open data ->
                    Just data.stratifier

                Closed ->
                    Nothing

        code =
            stratifier
                |> Maybe.andThen .code
                |> Maybe.andThen .text

        onSave_ =
            MaybeExtra.andMap stratifier onSave
    in
    dialog
        { dialogConfig
            | open = isOpen model
            , onClose = Just (onMsg ClickedClose)
            , additionalAttributes = [ class "measure-stratifier-dialog" ]
        }
        { title = Just "Stratifier"
        , content =
            [ codeField onSave_ onMsg code
            , stratifier
                |> Maybe.andThen .description
                |> descriptionField onSave_ onMsg
            , stratifier
                |> Maybe.andThen .criteria
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
