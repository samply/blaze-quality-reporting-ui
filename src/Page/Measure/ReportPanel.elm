module Page.Measure.ReportPanel exposing (Model, Msg, init, update, view)

import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp exposing (Error(..))
import Fhir.Measure exposing (Measure)
import Fhir.MeasureReport as MeasureReport exposing (MeasureReport, Status(..))
import Fhir.OperationOutcome as OperationOutcome
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html, div, h3, h4, h5, li, p, text, ul)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Loading
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.LayoutGrid as LayoutGrid exposing (span12)
import Material.List as List
import Material.List.Item as ListItem
import MaterialUtil
import Maybe.Extra as MaybeExtra
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { base : String
    , measure : Measure
    , reports : Loading.Status (List MeasureReport)
    , error : Maybe FhirHttp.Error
    }


init : String -> Measure -> ( Model, Cmd Msg )
init base measure =
    ( { base = base
      , measure = measure
      , reports = Loading.Loading
      , error = Nothing
      }
    , measure.url
        |> Maybe.map (\url -> loadReports base url measure.version)
        |> Maybe.withDefault Cmd.none
    )



-- UPDATE


type Msg
    = ClickedGenerate
    | ClickedErrorDialogClose
    | CompletedLoadReports (Result FhirHttp.Error Bundle)
    | CompletedGenerateReport (Result FhirHttp.Error MeasureReport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGenerate ->
            ( model
            , case model.measure.url of
                Just url ->
                    FhirHttp.postOperationType CompletedGenerateReport
                        model.base
                        "Measure"
                        "$evaluate-measure"
                        [ UrlBuilder.string "measure" url
                        , UrlBuilder.string "periodStart" "1900"
                        , UrlBuilder.string "periodEnd" "2100"
                        ]
                        MeasureReport.decoder

                Nothing ->
                    Cmd.none
            )

        ClickedErrorDialogClose ->
            ( { model | error = Nothing }, Cmd.none )

        CompletedLoadReports (Ok bundle) ->
            ( { model | reports = Loading.Loaded (decodeReports bundle) }
            , Cmd.none
            )

        CompletedLoadReports (Err error) ->
            ( { model | reports = Loading.Failed error }
            , Cmd.none
            )

        CompletedGenerateReport (Ok report) ->
            ( addReport report model
            , Cmd.none
            )

        CompletedGenerateReport (Err error) ->
            ( { model | error = Just error }
            , Cmd.none
            )


addReport report model =
    case model.reports of
        Loading.Loaded reports ->
            { model | reports = Loading.Loaded (report :: reports) }

        Loading.Loading ->
            model

        Loading.LoadingSlowly ->
            model

        Loading.Failed _ ->
            model


loadReports base url version =
    let
        search =
            version
                |> Maybe.map ((++) (url ++ "|"))
                |> Maybe.withDefault url
    in
    FhirHttp.searchType CompletedLoadReports
        base
        "MeasureReport"
        [ UrlBuilder.string "measure" search ]


decodeReports : Bundle -> List MeasureReport
decodeReports { entry } =
    List.filterMap
        (.resource >> decodeValue MeasureReport.decoder >> Result.toMaybe)
        entry



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onLibraryAssoc : msg
    , onReportClick : Id -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    LayoutGrid.cell [ span12, class "measure-report-panel" ]
        [ viewErrorDialog model.error |> Html.map config.onMsg
        , h3
            [ class "measure-report-panel__title"
            , class "mdc-typography--headline5"
            ]
            [ text "Reports" ]
        , case ( model.measure.url, List.head model.measure.library ) of
            ( Just _, Just _ ) ->
                case model.reports of
                    Loading.Loaded reports ->
                        if List.isEmpty reports then
                            emptyListPlaceholder config.onMsg

                        else
                            div []
                                [ viewReportList config.onReportClick reports
                                , generateButton config.onMsg
                                ]

                    Loading.Loading ->
                        text ""

                    Loading.LoadingSlowly ->
                        text ""

                    Loading.Failed _ ->
                        text "error"

            ( Nothing, _ ) ->
                missingUrlMessage

            ( _, Nothing ) ->
                missingLibraryMessage config.onLibraryAssoc
        ]


missingUrlMessage =
    div []
        [ text "Please assign an URL to the measure before generating a report."
        ]


missingLibraryMessage onLibraryAssoc =
    div []
        [ text "Please"
        , Button.text
            (Button.config |> Button.setOnClick onLibraryAssoc)
            "associate"
        , text "a library before generating a report."
        ]


emptyListPlaceholder onMsg =
    div [ class "measure-report-panel__empty-placeholder" ]
        [ generateButton onMsg ]


generateButton onMsg =
    Button.outlined
        (Button.config |> Button.setOnClick (onMsg ClickedGenerate))
        "Generate First Report"


viewReportList onReportClick reports =
    List.list List.config <|
        List.map (viewReport onReportClick) reports


viewReport onReportClick ({ status, date } as report) =
    ListItem.listItem
        (ListItem.config
            |> MaterialUtil.liftMaybe ListItem.setOnClick (Maybe.map onReportClick report.id)
            |> ListItem.setDisabled (MaybeExtra.isNothing report.id)
        )
        [ ListItem.graphic [] [ statusIcon status ]
        , text (Maybe.withDefault "<unknown-date>" date)
        ]


statusIcon status =
    Icon.icon []
        (case status of
            Complete ->
                "done"

            Pending ->
                "av_timer"

            Error ->
                "error_outline"
        )


viewErrorDialog error =
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen (MaybeExtra.isJust error)
            |> Dialog.setOnClose ClickedErrorDialogClose
        )
        { title = Just "Error"
        , content = error |> Maybe.map errorDialogContent |> Maybe.withDefault []
        , actions = [ errorDialogOkButton ]
        }


errorDialogContent : FhirHttp.Error -> List (Html Msg)
errorDialogContent error =
    case error of
        BadUrl url ->
            [ text ("Bad URL " ++ url) ]

        Timeout ->
            [ text "Timeout. Please try again." ]

        NetworkError ->
            [ text "Network Error. Please try again." ]

        BadStatus _ operationOutcome ->
            List.map
                (\issue ->
                    div []
                        [ h4 [ class "mdc-typography--headline6" ]
                            [ text (issueTitle issue.code) ]
                        , p [] [ text (Maybe.withDefault "" issue.diagnostics) ]
                        , h5 [ class "mdc-typography--subtitle1" ]
                            [ text "Location:" ]
                        , ul [] <|
                            List.map
                                (\expression -> li [] [ text expression ])
                                issue.expression
                        ]
                )
                operationOutcome.issue

        BadBody s ->
            [ text ("Invalid response from the FHIR server: " ++ s) ]


issueTitle issueType =
    case issueType of
        OperationOutcome.Required ->
            "Missing required element"

        OperationOutcome.Value ->
            "Invalid element or header"

        OperationOutcome.Exception ->
            "Unexpected internal error"

        _ ->
            "Other error"


errorDialogOkButton =
    Button.text
        (Button.config |> Button.setOnClick ClickedErrorDialogClose)
        "Ok"
