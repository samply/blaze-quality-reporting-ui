module Page.Measure exposing (Model, Msg, init, toSession, update, view)

import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import List.Extra exposing (getAt, removeAt, setAt, updateAt)
import Loading exposing (Status(..))
import Material.Button exposing (buttonConfig, outlinedButton)
import Material.Card
    exposing
        ( card
        , cardActionButton
        , cardActions
        , cardBlock
        , cardConfig
        )
import Material.LayoutGrid
    exposing
        ( layoutGrid
        , layoutGridCell
        , layoutGridInner
        , span12
        )
import Page.Measure.Header as Header
import Page.Measure.ReportPanel as ReportPanel
import Page.Measure.StratifierDialog as StratifierDialog
import Route exposing (href)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , stratifierDialog : StratifierDialog.Model
    , measureId : Id
    , data : Status Data
    , onStratifierSave : Maybe (Measure.Stratifier -> Msg)
    }


type alias Data =
    { measure : Measure
    , header : Header.Model
    , reportPanel : ReportPanel.Model
    }


init : Session -> Id -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , stratifierDialog = StratifierDialog.init
      , measureId = id
      , data = Loading
      , onStratifierSave = Nothing
      }
    , loadMeasure session.base id
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ClickedHeaderSave (Maybe String) (Maybe String)
    | ClickedHeaderDelete
    | ClickedStratifierEdit Int Int
    | ClickedStratifierDelete Int Int
    | ClickedAddStratifier Int
    | ClickedStratifierSaveAtAdd Int Measure.Stratifier
    | ClickedStratifierSaveAtUpdate Int Int Measure.Stratifier
    | CompletedLoadMeasure (Result Http.Error Measure)
    | CompletedSaveMeasure (Result Http.Error Measure)
    | CompletedDeleteMeasure (Result Http.Error ())
    | StratifierDialogMsg StratifierDialog.Msg
    | HeaderMsg Header.Msg
    | ReportPanelMsg ReportPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedHeaderSave title description ->
            ( model
            , doWithData
                (\{ measure } ->
                    { measure
                        | title = title
                        , description = description
                    }
                        |> saveMeasure model.session.base model.measureId
                )
                model
            )

        ClickedHeaderDelete ->
            ( model
            , deleteMeasure model.session.base model.measureId
            )

        ClickedStratifierEdit groupIdx stratifierIdx ->
            ( case model.data of
                Loaded { measure } ->
                    let
                        maybeStratifier =
                            measure.group
                                |> getAt groupIdx
                                |> Maybe.map .stratifier
                                |> Maybe.andThen (getAt stratifierIdx)
                    in
                    case Debug.log "maybeStratifier" maybeStratifier of
                        Just stratifier ->
                            updateStratifierDialog
                                (StratifierDialog.doOpen stratifier)
                                { model
                                    | onStratifierSave =
                                        ClickedStratifierSaveAtUpdate
                                            groupIdx
                                            stratifierIdx
                                            |> Just
                                }

                        Nothing ->
                            model

                _ ->
                    model
            , Cmd.none
            )

        ClickedStratifierDelete groupIdx stratifierIdx ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (deleteStratifier stratifierIdx) groupIdx measure
                        |> saveMeasure model.session.base model.measureId
                )
                model
            )

        ClickedAddStratifier groupIdx ->
            ( updateStratifierDialog
                (StratifierDialog.doOpen Measure.newStratifier)
                { model
                    | onStratifierSave =
                        ClickedStratifierSaveAtAdd
                            groupIdx
                            |> Just
                }
            , Cmd.none
            )

        ClickedStratifierSaveAtAdd groupIdx stratifier ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (addStratifier stratifier)
                        groupIdx
                        measure
                        |> saveMeasure model.session.base model.measureId
                )
                model
            )

        ClickedStratifierSaveAtUpdate groupIdx stratifierIdx stratifier ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (setStratifier stratifierIdx stratifier)
                        groupIdx
                        measure
                        |> saveMeasure model.session.base model.measureId
                )
                model
            )

        CompletedLoadMeasure (Ok measure) ->
            let
                ( data, cmd ) =
                    loaded model.session.base model.measureId measure
            in
            ( { model | data = data }, Cmd.map ReportPanelMsg cmd )

        CompletedLoadMeasure (Err _) ->
            ( { model | data = Failed }
            , Cmd.none
            )

        CompletedSaveMeasure (Ok measure) ->
            let
                ( data, cmd ) =
                    loaded model.session.base model.measureId measure
            in
            ( updateStratifierDialog StratifierDialog.doClose
                { model | data = data }
            , Cmd.map ReportPanelMsg cmd
            )

        CompletedSaveMeasure (Err _) ->
            ( model, Cmd.none )

        CompletedDeleteMeasure (Ok _) ->
            ( model
            , Route.pushUrl (Session.navKey model.session) Route.MeasureList
            )

        CompletedDeleteMeasure (Err _) ->
            ( model, Cmd.none )

        StratifierDialogMsg msg_ ->
            ( updateStratifierDialog (StratifierDialog.update msg_) model
            , Cmd.none
            )

        HeaderMsg msg_ ->
            ( updateHeader (Header.update msg_) model, Cmd.none )

        ReportPanelMsg msg_ ->
            updateReportPanel (ReportPanel.update msg_) model


doWithData : (Data -> Cmd Msg) -> Model -> Cmd Msg
doWithData f model =
    case model.data of
        Loaded data ->
            f data

        _ ->
            Cmd.none


addStratifier : Measure.Stratifier -> Measure.Group -> Measure.Group
addStratifier stratifier group =
    { group | stratifier = group.stratifier ++ [ stratifier ] }


setStratifier : Int -> Measure.Stratifier -> Measure.Group -> Measure.Group
setStratifier idx stratifier group =
    { group | stratifier = setAt idx stratifier group.stratifier }


deleteStratifier : Int -> Measure.Group -> Measure.Group
deleteStratifier idx group =
    { group | stratifier = removeAt idx group.stratifier }


updateGroup : (Measure.Group -> Measure.Group) -> Int -> Measure -> Measure
updateGroup f groupIdx measure =
    { measure | group = updateAt groupIdx f measure.group }


updateHeader : (Header.Model -> Header.Model) -> Model -> Model
updateHeader f =
    updateData (\data -> { data | header = f data.header })


updateReportPanel :
    (ReportPanel.Model -> ( ReportPanel.Model, Cmd ReportPanel.Msg ))
    -> Model
    -> ( Model, Cmd Msg )
updateReportPanel f =
    updateDataWithCmd
        (\data ->
            let
                ( reportPanel, cmd ) =
                    f data.reportPanel
            in
            ( { data | reportPanel = reportPanel }
            , Cmd.map ReportPanelMsg cmd
            )
        )


updateData f model =
    case model.data of
        Loaded data ->
            { model | data = Loaded (f data) }

        _ ->
            model


updateDataWithCmd f model =
    case model.data of
        Loaded data ->
            let
                ( data_, cmd ) =
                    f data
            in
            ( { model | data = Loaded data_ }, cmd )

        _ ->
            ( model, Cmd.none )


updateStratifierDialog f model =
    { model | stratifierDialog = f model.stratifierDialog }


loaded base measureId measure =
    let
        ( reportPanel, cmd ) =
            ReportPanel.init base measureId
    in
    ( Loaded
        { measure = measure
        , header = Header.init measure.title measure.description
        , reportPanel = reportPanel
        }
    , cmd
    )


loadMeasure base id =
    FhirHttp.read CompletedLoadMeasure base "Measure" id Measure.decoder


saveMeasure base measureId measure =
    FhirHttp.update CompletedSaveMeasure
        base
        "Measure"
        measureId
        Measure.decoder
        (Measure.encode measure)


deleteMeasure base measureId =
    FhirHttp.delete CompletedDeleteMeasure base "Measure" measureId



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    case model.data of
        Loaded measure ->
            { title = [ "Measure" ]
            , content =
                div []
                    [ viewStratifierDialog model
                    , viewMeasure measure
                    ]
            }

        Loading ->
            { title = [ "Measure" ]
            , content =
                div []
                    [ viewStratifierDialog model
                    ]
            }

        LoadingSlowly ->
            { title = [ "Measure" ]
            , content =
                div []
                    [ viewStratifierDialog model
                    ]
            }

        Failed ->
            { title = [ "Measure" ]
            , content =
                div []
                    [ viewStratifierDialog model
                    ]
            }


viewStratifierDialog : Model -> Html Msg
viewStratifierDialog model =
    StratifierDialog.view
        { onSave = model.onStratifierSave
        , onMsg = StratifierDialogMsg
        }
        model.stratifierDialog


viewMeasure : Data -> Html Msg
viewMeasure { measure, header, reportPanel } =
    layoutGrid [ class "measure" ]
        [ layoutGridInner []
            ([ viewHeader header ]
                ++ List.indexedMap (\idx group -> viewGroup idx group)
                    measure.group
                ++ [ ReportPanel.view reportPanel |> Html.map ReportPanelMsg ]
            )
        ]


viewHeader header =
    Header.view
        { onSave = ClickedHeaderSave
        , onDelete = ClickedHeaderDelete
        , onMsg = HeaderMsg
        }
        header


libraryLink : List Canonical -> Html Msg
libraryLink uris =
    uris
        |> List.head
        |> Maybe.map (\uri -> a [ href (Route.LibraryByUrl uri) ] [ text uri ])
        |> Maybe.withDefault (text "no associated library")


viewGroup : Int -> Measure.Group -> Html Msg
viewGroup groupIdx { stratifier } =
    layoutGridCell [ span12 ]
        [ layoutGridInner [] <|
            List.indexedMap
                (\stratifierIdx s ->
                    layoutGridCell []
                        [ viewStratifier groupIdx stratifierIdx s ]
                )
                stratifier
                ++ [ layoutGridCell [ span12 ]
                        [ outlinedButton
                            { buttonConfig
                                | onClick = Just (ClickedAddStratifier groupIdx)
                            }
                            "add stratifier"
                        ]
                   ]
        ]


viewStratifier : Int -> Int -> Measure.Stratifier -> Html Msg
viewStratifier groupIdx stratifierIdx { code, description } =
    card
        { cardConfig
            | outlined = True
            , additionalAttributes = [ class "measure-stratifier" ]
        }
        { blocks =
            [ cardBlock <|
                div [ class "measure-stratifier__header" ]
                    [ h4
                        [ class "measure-stratifier__title"
                        , class "mdc-typography--headline6"
                        ]
                        [ code
                            |> Maybe.andThen .text
                            |> Maybe.withDefault
                                ("Stratifier " ++ String.fromInt (stratifierIdx + 1))
                            |> text
                        ]
                    ]
            , cardBlock <|
                div [ class "measure-stratifier__description" ]
                    [ text (Maybe.withDefault "" description) ]
            ]
        , actions =
            Just <|
                cardActions
                    { buttons =
                        [ cardActionButton
                            { buttonConfig
                                | onClick =
                                    ClickedStratifierEdit
                                        groupIdx
                                        stratifierIdx
                                        |> Just
                            }
                            "edit"
                        , cardActionButton
                            { buttonConfig
                                | onClick =
                                    ClickedStratifierDelete
                                        groupIdx
                                        stratifierIdx
                                        |> Just
                            }
                            "delete"
                        ]
                    , icons = []
                    }
        }
