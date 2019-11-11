module Library.Panel exposing (Model, Msg, init, update, view)

import Fhir.Library as Library exposing (Library)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Library.Form as Form
import Library.LoadDialog as LoadDialog
import Material.Button exposing (buttonConfig, raisedButton)
import Random
import Task
import Uuid exposing (Uuid, uuidGenerator)


type alias Model msg =
    { base : String
    , onMsg : Msg -> msg
    , onLibraryChanged : Library -> msg
    , loadDialog : LoadDialog.Model
    , form : Maybe Form.Model
    }


init : String -> (Msg -> msg) -> (Library -> msg) -> Model msg
init base onMsg onLibraryChanged =
    { base = base
    , onMsg = onMsg
    , onLibraryChanged = onLibraryChanged
    , loadDialog = LoadDialog.init base
    , form = Nothing
    }


type Msg
    = ClickedLoad
    | ClickedCreate
    | ReceivedRandomUuid Uuid
    | LoadedLibrary Library
    | ClosedForm
    | LoadDialogMsg LoadDialog.Msg
    | FormMsg Form.Msg


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        ClickedLoad ->
            let
                ( loadDialog, cmd ) =
                    LoadDialog.doOpen model.loadDialog
            in
            ( { model | loadDialog = loadDialog }
            , Cmd.map (LoadDialogMsg >> model.onMsg) cmd
            )

        LoadedLibrary library ->
            ( { model
                | loadDialog = LoadDialog.doClose model.loadDialog
                , form = Just <| Form.init model.base library
              }
            , Task.perform model.onLibraryChanged (Task.succeed library)
            )

        ClosedForm ->
            ( { model | form = Nothing }
            , Cmd.none
            )

        ClickedCreate ->
            ( model
            , Random.generate ReceivedRandomUuid uuidGenerator
                |> Cmd.map model.onMsg
            )

        ReceivedRandomUuid uuid ->
            ( { model
                | form = Just <| Form.init model.base <| initLibrary uuid
              }
            , Cmd.none
            )

        LoadDialogMsg msg_ ->
            let
                ( loadDialog, cmd ) =
                    LoadDialog.update msg_ model.loadDialog
            in
            ( { model | loadDialog = loadDialog }
            , Cmd.map (LoadDialogMsg >> model.onMsg) cmd
            )

        FormMsg msg_ ->
            case model.form of
                Just form ->
                    let
                        ( form_, cmd ) =
                            Form.update msg_ form
                    in
                    ( { model | form = Just form_ }
                    , Cmd.map (FormMsg >> model.onMsg) cmd
                    )

                Nothing ->
                    ( model, Cmd.none )


initLibrary : Uuid -> Library
initLibrary uuid =
    Library.new <| "urn:uuid:" ++ Uuid.toString uuid


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


view : Model msg -> Html Msg
view { loadDialog, form } =
    div []
        [ h2 [] [ text "Library" ]
        , LoadDialog.view
            { onMsg = LoadDialogMsg, onSelect = LoadedLibrary }
            loadDialog
        , case form of
            Just library_ ->
                Form.view
                    { onMsg = FormMsg, onClose = ClosedForm }
                    library_

            Nothing ->
                loadOrCreatePanel
        ]
