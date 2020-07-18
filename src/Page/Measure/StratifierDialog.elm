module Page.Measure.StratifierDialog exposing
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
import Fhir.Expression as Expression
import Fhir.Measure as Measure
import Fhir.Measure.Stratifier exposing (newComponent)
import Html exposing (Html)
import Html.Attributes exposing (class)
import List.Extra exposing (removeAt, updateAt)
import Maybe.Extra as MaybeExtra
import Page.Measure.StratifierDialog.ComponentForm as ComponentForm
import Util



-- MODEL


type Model
    = Closed
    | Open { componentForms : List ComponentForm.Model }


init : Model
init =
    Closed



-- UPDATE


type Msg
    = ClickedClose
    | ClickedAddComponent
    | ClickedRemoveComponent Int
    | GotComponentFormMsg Int ComponentForm.Msg


doOpen : Measure.Stratifier -> Model -> Model
doOpen stratifier _ =
    if List.isEmpty stratifier.component then
        Open
            { componentForms =
                [ ComponentForm.init
                    { code = stratifier.code
                    , description = stratifier.description
                    , criteria =
                        stratifier.criteria
                            |> Maybe.withDefault (Expression.cql Nothing)
                    }
                ]
            }

    else
        Open
            { componentForms =
                List.map ComponentForm.init stratifier.component
            }


doClose : Model -> Model
doClose _ =
    Closed


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedClose ->
            doClose model

        ClickedAddComponent ->
            addComponentForm (ComponentForm.init newComponent) model

        ClickedRemoveComponent idx ->
            removeComponentForm idx model

        GotComponentFormMsg idx msg_ ->
            updateComponentForm idx (ComponentForm.update msg_) model


addComponentForm form =
    updateData
        (\data ->
            { data | componentForms = data.componentForms ++ [ form ] }
        )


updateComponentForm idx f =
    updateData
        (\data ->
            { data | componentForms = updateAt idx f data.componentForms }
        )


removeComponentForm idx =
    updateData
        (\data ->
            { data | componentForms = removeAt idx data.componentForms }
        )


updateData f model =
    case model of
        Open data ->
            Open (f data)

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
        componentForms =
            case model of
                Open data ->
                    data.componentForms

                Closed ->
                    []

        onSave_ =
            MaybeExtra.andMap (toStratifier componentForms) onSave
    in
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen (isOpen model)
            |> Dialog.setOnClose (onMsg ClickedClose)
            |> Dialog.setAttributes [ class "measure-stratifier-dialog" ]
        )
        { title = Just "Stratifier"
        , content =
            List.indexedMap (viewComponentForm onMsg onSave_ (List.length componentForms - 1)) componentForms
        , actions =
            [ Button.secondary
                (Button.config
                    |> Button.setOnClick (onMsg ClickedClose)
                )
                "Cancel"
            , Button.primary
                (Button.config
                    |> Util.liftMaybe Button.setOnClick onSave_
                    |> Button.setDisabled (not (isValid model))
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


isValid : Model -> Bool
isValid model =
    case model of
        Open data ->
            List.all ComponentForm.isValid data.componentForms

        Closed ->
            False


viewComponentForm : (Msg -> msg) -> Maybe msg -> Int -> Int -> ComponentForm.Model -> Html msg
viewComponentForm onMsg onSave lastIdx idx model =
    ComponentForm.view
        { onMsg = GotComponentFormMsg idx >> onMsg
        , control =
            if idx == lastIdx then
                ComponentForm.AddComponent (onMsg ClickedAddComponent)

            else
                ComponentForm.RemoveComponent (onMsg (ClickedRemoveComponent idx))
        , onSave = onSave
        }
        model


toStratifier : List ComponentForm.Model -> Maybe Measure.Stratifier
toStratifier forms =
    case forms of
        [] ->
            Nothing

        [ oneForm ] ->
            let
                component =
                    ComponentForm.toStratifierComponent oneForm
            in
            Just
                { code = component.code
                , description = component.description
                , criteria = Just component.criteria
                , component = []
                }

        multipleForms ->
            Just
                { code = Nothing
                , description = Nothing
                , criteria = Nothing
                , component =
                    List.map ComponentForm.toStratifierComponent multipleForms
                }
