module Measure.Panel exposing (Model, Msg, init, update, updateLibraryUrl, view)

import Fhir.Measure as Measure exposing (Measure)
import Fhir.PrimitiveTypes exposing (Uri)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Material.Button exposing (buttonConfig, raisedButton)
import Measure.Form as Form
import Measure.LoadDialog as LoadDialog


type alias Model =
    { base : String
    , loadDialog : LoadDialog.Model
    , form : Maybe Form.Model
    , libraryUrl : Maybe Uri
    }


init : String -> Maybe Uri -> Model
init base libraryUrl =
    { base = base
    , loadDialog = LoadDialog.init base
    , form = Nothing
    , libraryUrl = libraryUrl
    }


type Msg
    = ClickedLoad
    | ClickedCreate
    | LoadedMeasure Measure
    | ClosedForm
    | LoadDialogMsg LoadDialog.Msg
    | FormMsg Form.Msg


updateLibraryUrl : Maybe Uri -> Model -> Model
updateLibraryUrl libraryUrl model =
    { model | libraryUrl = Debug.log "updateLibraryUrl" libraryUrl }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLoad ->
            let
                ( loadDialog, cmd ) =
                    LoadDialog.doOpen model.loadDialog
            in
            ( { model | loadDialog = loadDialog }
            , Cmd.map LoadDialogMsg cmd
            )

        LoadedMeasure measure ->
            ( { model
                | loadDialog = LoadDialog.doClose model.loadDialog
                , form = Just <| Form.init model.base measure
              }
            , Cmd.none
            )

        ClosedForm ->
            ( { model | form = Nothing }
            , Cmd.none
            )

        ClickedCreate ->
            ( { model
                | form =
                    Just <|
                        Form.init model.base <|
                            Measure.new model.libraryUrl
              }
            , Cmd.none
            )

        LoadDialogMsg msg_ ->
            let
                ( loadDialog, cmd ) =
                    LoadDialog.update msg_ model.loadDialog
            in
            ( { model | loadDialog = loadDialog }
            , Cmd.map LoadDialogMsg cmd
            )

        FormMsg msg_ ->
            case model.form of
                Just form ->
                    let
                        ( form_, cmd ) =
                            Form.update msg_ form
                    in
                    ( { model | form = Just form_ }
                    , Cmd.map FormMsg cmd
                    )

                Nothing ->
                    ( model, Cmd.none )


loadOrCreatePanel =
    div [ class "load-or-create-panel" ]
        [ raisedButton
            { buttonConfig
                | onClick = Just ClickedLoad
            }
            "Load"
        , raisedButton
            { buttonConfig
                | onClick = Just ClickedCreate
                , additionalAttributes = [ style "margin-left" "1em" ]
            }
            "Create"
        ]


view : Model -> Html Msg
view { loadDialog, form } =
    div []
        [ h2 [] [ text "Measure" ]
        , LoadDialog.view
            { onMsg = LoadDialogMsg, onSelect = LoadedMeasure }
            loadDialog
        , case form of
            Just measure_ ->
                Form.view
                    { onMsg = FormMsg, onClose = ClosedForm }
                    measure_

            Nothing ->
                loadOrCreatePanel
        ]
