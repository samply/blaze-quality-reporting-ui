module Page.Measure exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , updateSession
    , view
    )

import Component.Button as Button
import Component.Error as Error
import Component.Header as Header
import Component.List as List
import Component.List.Item as ListItem exposing (ListItem)
import Fhir.Http as FhirHttp
import Fhir.Library exposing (Library)
import Fhir.Measure as Measure exposing (Measure)
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class)
import Http
import List.Extra as ListExtra
import List.Zipper as Zipper
import Loading exposing (Status(..))
import Maybe.Extra as MaybeExtra
import Page.Measure.AssocLibraryDialog as AssocLibraryDialog
import Page.Measure.PopulationDialog as PopulationDialog
import Page.Measure.ReportPanel as ReportPanel
import Page.Measure.Sidebar as Sidebar
import Page.Measure.StratifierDialog as StratifierDialog
import Route exposing (Route)
import Session exposing (Server, Session)



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
    , sidebar : Sidebar.Model
    , reportPanel : ReportPanel.Model
    }


init : Session -> Id -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , populationDialog = PopulationDialog.init
      , stratifierDialog = StratifierDialog.init
      , assocLibraryDialog = AssocLibraryDialog.init (Session.getBase session)
      , measureId = id
      , data = Loading
      , onPopulationSave = Nothing
      , onStratifierSave = Nothing
      }
    , loadMeasure (Session.getBase session) id
    )


toSession : Model -> Session
toSession model =
    model.session


updateSession : (Session -> Session) -> Model -> Model
updateSession f model =
    { model | session = f model.session }



-- UPDATE


type Msg
    = ClickedHeaderSave (Maybe String) (Maybe String)
    | ClickedHeaderDelete
    | ClickedPopulationEdit Int Int
    | ClickedStratifierEdit Int Int
    | ClickedStratifierDelete Int Int
    | ClickedStratifierMoveUp Int Int
    | ClickedStratifierMoveDown Int Int
    | ClickedAddStratifier Int
    | ClickedLibraryAssoc
    | ClickedSidebarLibraryEdit
    | ClickedSidebarSave Measure
    | ClickedSidebarCopyToServer Server
    | ClickedPopulationSaveAtUpdate Int Int Measure.Population
    | ClickedStratifierSaveAtAdd Int Measure.Stratifier
    | ClickedStratifierSaveAtUpdate Int Int Measure.Stratifier
    | ClickedReport Id
    | SelectedLibrary Library
    | CompletedLoadMeasure (Result FhirHttp.Error Measure)
    | CompletedSaveMeasure (Result FhirHttp.Error Measure)
    | CompletedDuplicateMeasure (Result FhirHttp.Error Measure)
    | CompletedDeleteMeasure (Result Http.Error ())
    | GotPopulationDialogMsg PopulationDialog.Msg
    | GotStratifierDialogMsg StratifierDialog.Msg
    | GotAssocLibraryDialogMsg AssocLibraryDialog.Msg
    | GotHeaderMsg Header.Msg
    | GotSidebarMsg Sidebar.Msg
    | GotReportPanelMsg ReportPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Route )
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
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
            )

        ClickedHeaderDelete ->
            ( model
            , deleteMeasure (Session.getBase model.session) model.measureId
            , Nothing
            )

        ClickedPopulationEdit groupIdx populationIdx ->
            ( case model.data of
                Loaded { measure } ->
                    let
                        maybePopulation =
                            measure.group
                                |> ListExtra.getAt groupIdx
                                |> Maybe.map .population
                                |> Maybe.andThen (ListExtra.getAt populationIdx)
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
            , Nothing
            )

        ClickedStratifierEdit groupIdx stratifierIdx ->
            ( case model.data of
                Loaded { measure } ->
                    let
                        maybeStratifier =
                            measure.group
                                |> ListExtra.getAt groupIdx
                                |> Maybe.map .stratifier
                                |> Maybe.andThen (ListExtra.getAt stratifierIdx)
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
            , Nothing
            )

        ClickedStratifierDelete groupIdx stratifierIdx ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (deleteStratifier stratifierIdx) groupIdx measure
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
            )

        ClickedStratifierMoveUp groupIdx stratifierIdx ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (moveStratifierUp stratifierIdx) groupIdx measure
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
            )

        ClickedStratifierMoveDown groupIdx stratifierIdx ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (moveStratifierDown stratifierIdx) groupIdx measure
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
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
            , Nothing
            )

        ClickedLibraryAssoc ->
            updateAssocLibraryDialog AssocLibraryDialog.doOpen model

        ClickedSidebarLibraryEdit ->
            updateAssocLibraryDialog AssocLibraryDialog.doOpen model

        ClickedSidebarSave measure ->
            ( model
            , saveMeasure (Session.getBase model.session) model.measureId measure
            , Nothing
            )

        ClickedSidebarCopyToServer server ->
            ( model
            , case model.data of
                Loaded { measure } ->
                    duplicateMeasure server.url measure

                _ ->
                    Cmd.none
            , Nothing
            )

        ClickedPopulationSaveAtUpdate groupIdx populationIdx population ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (setPopulation populationIdx population)
                        groupIdx
                        measure
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
            )

        ClickedStratifierSaveAtAdd groupIdx stratifier ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (addStratifier stratifier)
                        groupIdx
                        measure
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
            )

        ClickedStratifierSaveAtUpdate groupIdx stratifierIdx stratifier ->
            ( model
            , doWithData
                (\{ measure } ->
                    updateGroup (setStratifier stratifierIdx stratifier)
                        groupIdx
                        measure
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
            )

        ClickedReport id ->
            ( model, Cmd.none, Just (Route.MeasureReport id) )

        SelectedLibrary library ->
            ( model
            , doWithData
                (\{ measure } ->
                    { measure | library = MaybeExtra.toList library.url }
                        |> saveMeasure (Session.getBase model.session) model.measureId
                )
                model
            , Nothing
            )

        CompletedLoadMeasure (Ok measure) ->
            loaded measure model

        CompletedLoadMeasure (Err error) ->
            ( { model | data = Failed error }
            , Cmd.none
            , Nothing
            )

        CompletedSaveMeasure (Ok measure) ->
            let
                ( newModel, cmd, maybeRoute ) =
                    loaded measure model
            in
            ( newModel
                |> updatePopulationDialog PopulationDialog.doClose
                |> updateStratifierDialog StratifierDialog.doClose
                |> closeAssocLibraryDialog
            , cmd
            , maybeRoute
            )

        CompletedSaveMeasure (Err _) ->
            ( model, Cmd.none, Nothing )

        CompletedDuplicateMeasure (Ok measure) ->
            let
                ( newModel, cmd, maybeRoute ) =
                    loaded measure model
            in
            ( newModel
                |> updatePopulationDialog PopulationDialog.doClose
                |> updateStratifierDialog StratifierDialog.doClose
                |> closeAssocLibraryDialog
            , cmd
            , maybeRoute
            )

        CompletedDuplicateMeasure (Err _) ->
            ( model, Cmd.none, Nothing )

        CompletedDeleteMeasure (Ok _) ->
            ( model
            , Cmd.none
            , Just Route.MeasureList
            )

        CompletedDeleteMeasure (Err _) ->
            ( model, Cmd.none, Nothing )

        GotPopulationDialogMsg msg_ ->
            ( updatePopulationDialog (PopulationDialog.update msg_) model
            , Cmd.none
            , Nothing
            )

        GotStratifierDialogMsg msg_ ->
            ( updateStratifierDialog (StratifierDialog.update msg_) model
            , Cmd.none
            , Nothing
            )

        GotAssocLibraryDialogMsg msg_ ->
            updateAssocLibraryDialog (AssocLibraryDialog.update msg_) model

        GotHeaderMsg msg_ ->
            let
                updateHeader f =
                    updateData (\data -> { data | header = f data.header })
            in
            ( updateHeader (Header.update msg_) model, Cmd.none, Nothing )

        GotSidebarMsg msg_ ->
            updateDataWithCmd
                (\data ->
                    let
                        ( sidebar, cmd, maybeRoute ) =
                            Sidebar.update msg_ data.sidebar
                    in
                    ( { data | sidebar = sidebar }
                    , Cmd.map GotSidebarMsg cmd
                    , maybeRoute
                    )
                )
                model

        GotReportPanelMsg msg_ ->
            updateDataWithCmd
                (\data ->
                    let
                        ( reportPanel, cmd ) =
                            ReportPanel.update msg_ data.reportPanel
                    in
                    ( { data | reportPanel = reportPanel }
                    , Cmd.map GotReportPanelMsg cmd
                    , Nothing
                    )
                )
                model


doWithData f model =
    case model.data of
        Loaded data ->
            f data

        _ ->
            Cmd.none


setPopulation : Int -> Measure.Population -> Measure.Group -> Measure.Group
setPopulation idx population group =
    { group | population = ListExtra.setAt idx population group.population }


addStratifier : Measure.Stratifier -> Measure.Group -> Measure.Group
addStratifier stratifier group =
    { group | stratifier = group.stratifier ++ [ stratifier ] }


setStratifier : Int -> Measure.Stratifier -> Measure.Group -> Measure.Group
setStratifier idx stratifier group =
    { group | stratifier = ListExtra.setAt idx stratifier group.stratifier }


deleteStratifier : Int -> Measure.Group -> Measure.Group
deleteStratifier idx group =
    { group | stratifier = ListExtra.removeAt idx group.stratifier }


moveStratifierUp : Int -> Measure.Group -> Measure.Group
moveStratifierUp idx group =
    case
        ( ListExtra.getAt idx group.stratifier
        , ListExtra.getAt (idx - 1) group.stratifier
        )
    of
        ( Just self, Just prev ) ->
            { group
                | stratifier =
                    group.stratifier
                        |> ListExtra.setAt (idx - 1) self
                        |> ListExtra.setAt idx prev
            }

        _ ->
            group


moveStratifierDown : Int -> Measure.Group -> Measure.Group
moveStratifierDown idx group =
    case
        ( ListExtra.getAt idx group.stratifier
        , ListExtra.getAt (idx + 1) group.stratifier
        )
    of
        ( Just self, Just next ) ->
            { group
                | stratifier =
                    group.stratifier
                        |> ListExtra.setAt (idx + 1) self
                        |> ListExtra.setAt idx next
            }

        _ ->
            group


updateGroup : (Measure.Group -> Measure.Group) -> Int -> Measure -> Measure
updateGroup f groupIdx measure =
    { measure | group = ListExtra.updateAt groupIdx f measure.group }


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
                ( data_, cmd, maybeRoute ) =
                    f data
            in
            ( { model | data = Loaded data_ }, cmd, maybeRoute )

        _ ->
            ( model, Cmd.none, Nothing )


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
    , Cmd.map GotAssocLibraryDialogMsg cmd
    , Nothing
    )


{-| Returns a loaded model with freshly initialized Sidebar and ReportPanel.
-}
loaded : Measure -> Model -> ( Model, Cmd Msg, Maybe Route )
loaded measure model =
    let
        base =
            Session.getBase model.session

        ( sidebar, sidebarCmd ) =
            Sidebar.init base measure

        ( reportPanel, reportPanelCmd ) =
            case model.data of
                Loaded data ->
                    if
                        data.measure.url
                            /= measure.url
                            || data.measure.library
                            /= measure.library
                    then
                        ReportPanel.init base measure

                    else
                        ( data.reportPanel, Cmd.none )

                _ ->
                    ReportPanel.init base measure
    in
    ( { model
        | data =
            Loaded
                { measure = measure
                , header = Header.init measure.title measure.description
                , sidebar = sidebar
                , reportPanel = reportPanel
                }
      }
    , Cmd.batch
        [ Cmd.map GotSidebarMsg sidebarCmd
        , Cmd.map GotReportPanelMsg reportPanelCmd
        ]
    , Nothing
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


duplicateMeasure base measure =
    FhirHttp.create CompletedDuplicateMeasure
        base
        "Measure"
        Measure.decoder
        (Measure.encode measure)


deleteMeasure base measureId =
    FhirHttp.delete CompletedDeleteMeasure base "Measure" measureId



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    { title = [ "Measure" ]
    , content =
        div [ class "mt-16 ml-48 mr-64 p-6 flex-grow bg-gray-100" ]
            ([ viewPopulationDialog model
             , viewStratifierDialog model
             , viewAssocLibraryDialog model
             ]
                ++ viewData model.session model.data
            )
    }


viewPopulationDialog : Model -> Html Msg
viewPopulationDialog model =
    PopulationDialog.view
        { onMsg = GotPopulationDialogMsg
        , onSave = model.onPopulationSave
        }
        model.populationDialog


viewStratifierDialog : Model -> Html Msg
viewStratifierDialog model =
    StratifierDialog.view
        { onMsg = GotStratifierDialogMsg
        , onSave = model.onStratifierSave
        }
        model.stratifierDialog


viewAssocLibraryDialog : Model -> Html Msg
viewAssocLibraryDialog model =
    AssocLibraryDialog.view
        { onMsg = GotAssocLibraryDialogMsg
        , onSelect = SelectedLibrary
        }
        model.assocLibraryDialog


viewData : Session -> Status Data -> List (Html Msg)
viewData session data =
    case data of
        Loading ->
            []

        LoadingSlowly ->
            []

        Loaded loadedData ->
            [ viewMeasure loadedData
            , viewSidebar session loadedData.sidebar
            ]

        Reloading _ ->
            []

        ReloadingSlowly _ ->
            []

        Failed error ->
            [ viewError error ]


viewMeasure : Data -> Html Msg
viewMeasure { measure, header, reportPanel } =
    div [ class "" ]
        ([ viewHeader header ]
            ++ List.indexedMap viewGroup measure.group
            ++ [ viewReportPanel reportPanel ]
        )


viewHeader header =
    Header.view
        { onSave = ClickedHeaderSave
        , onDelete = ClickedHeaderDelete
        , onMsg = GotHeaderMsg
        }
        header


viewGroup : Int -> Measure.Group -> Html Msg
viewGroup groupIdx { population, stratifier } =
    div [ class "" ]
        [ viewPopulationPanel groupIdx population
        , viewStratifierPanel groupIdx stratifier
        ]


viewPopulationPanel groupIdx populations =
    div [ class "mb-4" ] <|
        [ h3 [ class "text-lg mb-2" ] [ text "Populations" ]
        , List.list List.config
            (List.indexedMap
                (\idx population ->
                    viewPopulation groupIdx idx population
                )
                populations
            )
        ]


viewPopulation : Int -> Int -> Measure.Population -> ListItem Msg
viewPopulation groupIdx populationIdx { code, description } =
    ListItem.listItem
        (ListItem.config
            |> ListItem.setOnClick (ClickedPopulationEdit groupIdx populationIdx)
        )
        (String.fromInt populationIdx)
        [ code
            |> Maybe.andThen (.coding >> List.head)
            |> Maybe.andThen .code
            |> Maybe.withDefault
                ("Population " ++ String.fromInt (populationIdx + 1))
            |> text
        ]


viewStratifierPanel groupIdx stratifiers =
    div [ class "mb-4" ] <|
        [ h3 [ class "text-lg mb-2" ] [ text "Stratifiers" ]
        , List.list (List.config |> List.setAttributes [ class "mb-2" ])
            (List.indexedMap
                (\idx stratifier ->
                    viewStratifier groupIdx idx stratifier
                )
                stratifiers
            )
        , Button.secondary
            (Button.config
                |> Button.setOnClick (ClickedAddStratifier groupIdx)
            )
            "add stratifier"
        ]


viewStratifier : Int -> Int -> Measure.Stratifier -> ListItem Msg
viewStratifier groupIdx stratifierIdx { code, description, component } =
    let
        title =
            if List.isEmpty component then
                code
                    |> Maybe.andThen .text
                    |> Maybe.withDefault
                        ("Stratifier " ++ String.fromInt (stratifierIdx + 1))

            else
                component
                    |> List.filterMap (.code >> Maybe.andThen .text)
                    |> String.join ", "
    in
    ListItem.listItem
        ListItem.config
        (String.fromInt stratifierIdx)
        [ text title
        , ListItem.meta []
            [ Button.icon
                (Button.config
                    |> Button.setOnClick (ClickedStratifierEdit groupIdx stratifierIdx)
                )
                "edit"
            , Button.icon
                (Button.config
                    |> Button.setOnClick (ClickedStratifierMoveUp groupIdx stratifierIdx)
                )
                "arrow_upward"
            , Button.icon
                (Button.config
                    |> Button.setOnClick (ClickedStratifierMoveDown groupIdx stratifierIdx)
                )
                "arrow_downward"
            , Button.icon
                (Button.config
                    |> Button.setOnClick (ClickedStratifierDelete groupIdx stratifierIdx)
                )
                "delete"
            ]
        ]


viewReportPanel reportPanel =
    ReportPanel.view
        { onMsg = GotReportPanelMsg
        , onLibraryAssoc = ClickedLibraryAssoc
        , onReportClick = ClickedReport
        }
        reportPanel


viewSidebar : Session -> Sidebar.Model -> Html Msg
viewSidebar session sidebar =
    Sidebar.view
        { servers = Zipper.toList session.servers
        , onMsg = GotSidebarMsg
        , onSave = ClickedSidebarSave
        , onLibraryEdit = ClickedSidebarLibraryEdit
        , onCopyToServer = ClickedSidebarCopyToServer
        }
        sidebar


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
            Error.view error
