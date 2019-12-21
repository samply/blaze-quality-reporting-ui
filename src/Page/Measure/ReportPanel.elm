module Page.Measure.ReportPanel exposing (Model, Msg, init, update, view)

import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp exposing (Error(..))
import Fhir.MeasureReport as MeasureReport exposing (MeasureReport, Status(..))
import Fhir.OperationOutcome as OperationOutcome
import Fhir.PrimitiveTypes exposing (Canonical, Id, Uri)
import Html exposing (Html, div, h3, h4, h5, li, p, text, ul)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Loading
import Material.Button exposing (buttonConfig, outlinedButton, textButton)
import Material.Dialog exposing (dialog, dialogConfig)
import Material.Icon exposing (icon, iconConfig)
import Material.LayoutGrid exposing (layoutGridCell, span12)
import Material.List
    exposing
        ( list
        , listConfig
        , listItem
        , listItemConfig
        , listItemGraphic
        )
import Maybe.Extra as MaybeExtra
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { base : String
    , measureUrl : Maybe Uri
    , libraryRef : Maybe Canonical
    , reports : Loading.Status (List MeasureReport)
    , error : Maybe FhirHttp.Error
    }


init : String -> Maybe Uri -> Maybe Canonical -> ( Model, Cmd Msg )
init base measureUrl libraryRef =
    ( { base = base
      , measureUrl = measureUrl
      , libraryRef = libraryRef
      , reports = Loading.Loading
      , error = Nothing
      }
    , case ( measureUrl, libraryRef ) of
        ( Just url, Just _ ) ->
            loadReports base url

        _ ->
            Cmd.none
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
            , case model.measureUrl of
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


loadReports base measureUrl =
    FhirHttp.searchType CompletedLoadReports
        base
        "MeasureReport"
        [ UrlBuilder.string "measure" measureUrl ]


decodeReports : Bundle -> List MeasureReport
decodeReports { entry } =
    List.filterMap
        (.resource >> decodeValue MeasureReport.decoder >> Result.toMaybe)
        entry



-- VIEW


type alias Config msg =
    { onLibraryAssoc : msg
    , onReportClick : Id -> msg
    , onMsg : Msg -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    layoutGridCell [ span12, class "measure-report-panel" ]
        [ viewErrorDialog model.error |> Html.map config.onMsg
        , h3
            [ class "measure-report-panel__title"
            , class "mdc-typography--headline5"
            ]
            [ text "Reports" ]
        , case ( model.measureUrl, model.libraryRef ) of
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
        , textButton
            { buttonConfig | onClick = Just onLibraryAssoc }
            "associate"
        , text "a library before generating a report."
        ]


emptyListPlaceholder onMsg =
    div [ class "measure-report-panel__empty-placeholder" ]
        [ generateButton onMsg ]


generateButton onMsg =
    outlinedButton
        { buttonConfig | onClick = Just (onMsg ClickedGenerate) }
        "Generate First Report"


viewReportList onReportClick reports =
    list listConfig <|
        List.map (viewReport onReportClick) reports


viewReport onReportClick ({ status, date } as report) =
    listItem
        { listItemConfig
            | onClick = Maybe.map (\id -> onReportClick id) report.id
            , disabled = MaybeExtra.isNothing report.id
        }
        [ listItemGraphic [] [ statusIcon status ]
        , text (Maybe.withDefault "<unknown-date>" date)
        ]


statusIcon status =
    icon iconConfig
        (case status of
            Complete ->
                "done"

            Pending ->
                "av_timer"

            Error ->
                "error_outline"
        )


viewErrorDialog error =
    dialog
        { dialogConfig
            | open = MaybeExtra.isJust error
            , onClose = Just ClickedErrorDialogClose
        }
        { title = Just "Error"
        , content = error |> Maybe.map errorDialogContent |> Maybe.withDefault []
        , actions =
            [ textButton
                { buttonConfig
                    | onClick = Just ClickedErrorDialogClose
                }
                "Ok"
            ]
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
