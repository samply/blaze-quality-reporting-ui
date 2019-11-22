module Page.Measure exposing (Model, Msg, init, toSession, update, view)

import Component.Header as Header
import Component.Sidebar exposing (sidebar, sidebarConfig)
import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Http as FhirHttp
import Fhir.Library exposing (Library)
import Fhir.Measure as Measure exposing (Measure)
import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (Html, div, h3, h4, text)
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
import Maybe.Extra as MaybeExtra
import Page.Measure.AssocLibraryDialog as AssocLibraryDialog
import Page.Measure.PopulationDialog as PopulationDialog
import Page.Measure.ReportPanel as ReportPanel
import Page.Measure.Sidebar.Library as SidebarLibrary
import Page.Measure.StratifierDialog as StratifierDialog
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , populationDialog : PopulationDialog.Model
    , stratifierDialog : StratifierDialog.Model
    , assocLibraryDialog : AssocLibraryDialog.Model
    , measureId : Id
    , data : Status Data
    , onPopulationSave : Maybe (Measure.Population -> Msg)
    , onStratifierSave : Maybe (Measure.Stratifier -> Msg)
    }


type alias Data =
    { measure : Measure
    , header : Header.Model
    , sidebarLibrary : SidebarLibrary.Model
    , reportPanel : ReportPanel.Model
    }


init : Session -> Id -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , populationDialog = PopulationDialog.init
      , stratifierDialog = StratifierDialog.init
      , assocLibraryDialog = AssocLibraryDialog.init session.base
      , measureId = id
      , data = Loading
      , onPopulationSave = Nothing
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
    | ClickedPopulationEdit Int Int
    | ClickedStratifierEdit Int Int
    | ClickedStratifierDelete Int Int
    | ClickedAddStratifier Int
    | ClickedLibraryAssoc
    | ClickedSidebarLibraryEdit
    | ClickedPopulationSaveAtUpdate Int Int Measure.Population
    | ClickedStratifierSaveAtAdd Int Measure.Stratifier
    | ClickedStratifierSaveAtUpdate Int Int Measure.Stratifier
    | ClickedReport Id
    | SelectedLibrary Library
    | CompletedLoadMeasure (Result FhirHttp.Error Measure)
    | CompletedSaveMeasure (Result FhirHttp.Error Measure)
    | CompletedDeleteMeasure (Result Http.Error ())
    | PopulationDialogMsg PopulationDialog.Msg
    | StratifierDialogMsg StratifierDialog.Msg
    | AssocLibraryDialogMsg AssocLibraryDialog.Msg
    | HeaderMsg Header.Msg
    | SidebarLibraryMsg SidebarLibrary.Msg
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

        ClickedPopulationEdit groupIdx populationIdx ->
            ( case model.data of
                Loaded { measure } ->
                    let
                        maybePopulation =
                            measure.group
                                |> getAt groupIdx
                                |> Maybe.map .population
                                |> Maybe.andThen (getAt populationIdx)
                    in
                    case maybePopulation of
                        Just population ->
                            updatePopulationDialog
                                (PopulationDialog.doOpen population)
                                { model
                                    | onPopulationSave =
                                        ClickedPopulationSaveAtUpdate
                                            groupIdx
                                            populationIdx
                                            |> Just
                                }

                        Nothing ->
                            model

                _ ->
                    model
            , Cmd.none
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
                    case maybeStratifier of
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

        ClickedLibraryAssoc ->
            updateAssocLibraryDialog AssocLibraryDialog.doOpen model

        ClickedSidebarLibraryEdit ->
            updateAssocLibraryDialog AssocLibraryDialog.doOpen model

        ClickedPopulationSaveAtUpdate groupIdx populationIdx population ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (setPopulation populationIdx population)
                        groupIdx
                        measure
                        |> saveMeasure model.session.base model.measureId
                )
                model
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

        ClickedReport id ->
            ( model
            , Route.pushUrl (Session.navKey model.session)
                (Route.MeasureReport id)
            )

        SelectedLibrary library ->
            ( model
            , doWithData
                (\{ measure } ->
                    { measure | library = MaybeExtra.toList library.url }
                        |> saveMeasure model.session.base model.measureId
                )
                model
            )

        CompletedLoadMeasure (Ok measure) ->
            let
                ( data, cmd ) =
                    loaded model.session.base model.measureId measure
            in
            ( { model | data = data }, cmd )

        CompletedLoadMeasure (Err error) ->
            ( { model | data = Failed error }
            , Cmd.none
            )

        CompletedSaveMeasure (Ok measure) ->
            let
                ( data, cmd ) =
                    loaded model.session.base model.measureId measure
            in
            ( { model | data = data }
                |> updatePopulationDialog PopulationDialog.doClose
                |> updateStratifierDialog StratifierDialog.doClose
                |> closeAssocLibraryDialog
            , cmd
            )

        CompletedSaveMeasure (Err _) ->
            ( model, Cmd.none )

        CompletedDeleteMeasure (Ok _) ->
            ( model
            , Route.pushUrl (Session.navKey model.session) Route.MeasureList
            )

        CompletedDeleteMeasure (Err _) ->
            ( model, Cmd.none )

        PopulationDialogMsg msg_ ->
            ( updatePopulationDialog (PopulationDialog.update msg_) model
            , Cmd.none
            )

        StratifierDialogMsg msg_ ->
            ( updateStratifierDialog (StratifierDialog.update msg_) model
            , Cmd.none
            )

        AssocLibraryDialogMsg msg_ ->
            updateAssocLibraryDialog (AssocLibraryDialog.update msg_) model

        HeaderMsg msg_ ->
            let
                updateHeader f =
                    updateData (\data -> { data | header = f data.header })
            in
            ( updateHeader (Header.update msg_) model, Cmd.none )

        SidebarLibraryMsg msg_ ->
            let
                updateSidebarLibrary f =
                    updateData
                        (\data ->
                            { data | sidebarLibrary = f data.sidebarLibrary }
                        )
            in
            ( updateSidebarLibrary (SidebarLibrary.update msg_) model
            , Cmd.none
            )

        ReportPanelMsg msg_ ->
            let
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
            in
            updateReportPanel (ReportPanel.update msg_) model


doWithData : (Data -> Cmd Msg) -> Model -> Cmd Msg
doWithData f model =
    case model.data of
        Loaded data ->
            f data

        _ ->
            Cmd.none


setPopulation : Int -> Measure.Population -> Measure.Group -> Measure.Group
setPopulation idx population group =
    { group | population = setAt idx population group.population }


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


updatePopulationDialog f model =
    { model | populationDialog = f model.populationDialog }


updateStratifierDialog f model =
    { model | stratifierDialog = f model.stratifierDialog }


closeAssocLibraryDialog model =
    { model
        | assocLibraryDialog =
            AssocLibraryDialog.doClose model.assocLibraryDialog
    }


updateAssocLibraryDialog f model =
    let
        ( assocLibraryDialog, cmd ) =
            f model.assocLibraryDialog
    in
    ( { model | assocLibraryDialog = assocLibraryDialog }
    , Cmd.map AssocLibraryDialogMsg cmd
    )


loaded base measureId measure =
    let
        ( sidebarLibrary, sidebarLibraryCmd ) =
            SidebarLibrary.init base (List.head measure.library)

        ( reportPanel, reportPanelCmd ) =
            ReportPanel.init base measureId
    in
    ( Loaded
        { measure = measure
        , header = Header.init measure.title measure.description
        , sidebarLibrary = sidebarLibrary
        , reportPanel = reportPanel
        }
    , Cmd.batch
        [ sidebarLibraryCmd |> Cmd.map SidebarLibraryMsg
        , reportPanelCmd |> Cmd.map ReportPanelMsg
        ]
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
        Loaded data ->
            { title = [ "Measure" ]
            , content =
                div [ class "main-content measure-page" ]
                    [ viewPopulationDialog model
                    , viewStratifierDialog model
                    , viewAssocLibraryDialog model
                    , viewMeasure data
                    , viewSidebar data.sidebarLibrary
                    ]
            }

        Loading ->
            { title = [ "Measure" ]
            , content =
                div [ class "main-content measure-page" ]
                    [ viewPopulationDialog model
                    , viewStratifierDialog model
                    , viewAssocLibraryDialog model
                    ]
            }

        LoadingSlowly ->
            { title = [ "Measure" ]
            , content =
                div [ class "main-content measure-page" ]
                    [ viewPopulationDialog model
                    , viewStratifierDialog model
                    , viewAssocLibraryDialog model
                    ]
            }

        Failed error ->
            { title = [ "Measure" ]
            , content =
                div [ class "main-content measure-page measure-page--error" ]
                    [ viewPopulationDialog model
                    , viewStratifierDialog model
                    , viewAssocLibraryDialog model
                    , viewError error
                    ]
            }


viewPopulationDialog : Model -> Html Msg
viewPopulationDialog model =
    PopulationDialog.view
        { onMsg = PopulationDialogMsg
        , onSave = model.onPopulationSave
        }
        model.populationDialog


viewStratifierDialog : Model -> Html Msg
viewStratifierDialog model =
    StratifierDialog.view
        { onMsg = StratifierDialogMsg
        , onSave = model.onStratifierSave
        }
        model.stratifierDialog


viewAssocLibraryDialog : Model -> Html Msg
viewAssocLibraryDialog model =
    AssocLibraryDialog.view
        { onMsg = AssocLibraryDialogMsg
        , onSelect = SelectedLibrary
        }
        model.assocLibraryDialog


viewSidebar : SidebarLibrary.Model -> Html Msg
viewSidebar sidebarLibrary =
    let
        config =
            { onEdit = ClickedSidebarLibraryEdit }
    in
    sidebar sidebarConfig
        [ SidebarLibrary.view config sidebarLibrary ]


viewMeasure : Data -> Html Msg
viewMeasure { measure, header, reportPanel } =
    layoutGrid [ class "measure" ]
        [ layoutGridInner []
            ([ viewHeader header ]
                ++ List.indexedMap viewGroup measure.group
                ++ [ viewReportPanel measure reportPanel ]
            )
        ]


viewHeader header =
    Header.view
        { onSave = ClickedHeaderSave
        , onDelete = ClickedHeaderDelete
        , onMsg = HeaderMsg
        }
        header


viewGroup : Int -> Measure.Group -> Html Msg
viewGroup groupIdx { population, stratifier } =
    layoutGridCell [ span12, class "measure-group" ]
        [ viewPopulationPanel groupIdx population
        , viewStratifierPanel groupIdx stratifier
        ]


viewPopulationPanel groupIdx populations =
    layoutGridInner [ class "measure-population-panel" ] <|
        [ layoutGridCell [ span12 ]
            [ h3 [ class "mdc-typography--headline5" ] [ text "Populations" ] ]
        ]
            ++ List.indexedMap
                (\idx population ->
                    layoutGridCell []
                        [ viewPopulation groupIdx idx population ]
                )
                populations


viewPopulation : Int -> Int -> Measure.Population -> Html Msg
viewPopulation groupIdx populationIdx { code, description } =
    card
        { cardConfig
            | outlined = True
            , additionalAttributes = [ class "measure-population" ]
        }
        { blocks =
            [ cardBlock <|
                div [ class "measure-population__header" ]
                    [ h4
                        [ class "measure-population__title"
                        , class "mdc-typography--headline6"
                        ]
                        [ code
                            |> Maybe.andThen (.coding >> List.head)
                            |> Maybe.andThen .code
                            |> Maybe.withDefault
                                ("Population " ++ String.fromInt (populationIdx + 1))
                            |> text
                        ]
                    ]
            , cardBlock <|
                div [ class "measure-population__description" ]
                    [ text (Maybe.withDefault "" description) ]
            ]
        , actions =
            Just <|
                cardActions
                    { buttons =
                        [ cardActionButton
                            { buttonConfig
                                | onClick =
                                    ClickedPopulationEdit
                                        groupIdx
                                        populationIdx
                                        |> Just
                            }
                            "edit"
                        ]
                    , icons = []
                    }
        }


viewStratifierPanel groupIdx stratifiers =
    layoutGridInner [ class "measure-stratifier-panel" ] <|
        [ layoutGridCell [ span12 ]
            [ h3 [ class "mdc-typography--headline5" ] [ text "Stratifiers" ] ]
        ]
            ++ List.indexedMap
                (\idx stratifier ->
                    layoutGridCell []
                        [ viewStratifier groupIdx idx stratifier ]
                )
                stratifiers
            ++ [ layoutGridCell [ span12 ]
                    [ outlinedButton
                        { buttonConfig
                            | onClick = Just (ClickedAddStratifier groupIdx)
                        }
                        "add stratifier"
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


viewReportPanel measure reportPanel =
    let
        config =
            { ready = not (List.isEmpty measure.library)
            , onLibraryAssoc = ClickedLibraryAssoc
            , onReportClick = ClickedReport
            , onMsg = ReportPanelMsg
            }
    in
    ReportPanel.view config reportPanel


viewError : FhirHttp.Error -> Html Msg
viewError error =
    case error of
        FhirHttp.BadStatus status _ ->
            case status of
                404 ->
                    div [ class "error" ]
                        [ div [ class "error__big-http-status" ]
                            [ text "404" ]
                        , div [ class "error__big-http-status-message" ]
                            [ text "Not Found" ]
                        ]

                _ ->
                    div [ class "error" ]
                        [ text "Other Error" ]

        _ ->
            div [ class "error" ]
                [ text "Other Error" ]
