module Page.Measure.ReportPanel exposing (Model, Msg, init, update, view)

import Component.Button as Button
import Component.Dialog as Dialog
import Component.List as List
import Component.List.Item as ListItem exposing (ListItem)
import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp exposing (Error(..))
import Fhir.Measure exposing (Measure)
import Fhir.MeasureReport as MeasureReport exposing (MeasureReport, Status(..))
import Fhir.OperationOutcome as OperationOutcome
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html, div, h3, h4, h5, li, p, text, ul)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Loading exposing (Status(..))
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
        Loading ->
            model

        LoadingSlowly ->
            model

        Loaded reports ->
            { model | reports = Loading.Loaded (report :: reports) }

        Reloading _ ->
            model

        ReloadingSlowly _ ->
            model

        Failed _ ->
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
        (.resource
            >> Maybe.map (decodeValue MeasureReport.decoder)
            >> Maybe.andThen Result.toMaybe
        )
        entry



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onLibraryAssoc : msg
    , onReportClick : Id -> msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onLibraryAssoc, onReportClick } model =
    div [ class "mb-4" ]
        [ viewErrorDialog model.error |> Html.map onMsg
        , div [ class "flex justify-between mb-2" ]
            [ h3 [ class "text-lg mb-2" ] [ text "Reports" ]
            , generateButton onMsg "Generate New Report"
            ]
        , case ( model.measure.url, List.head model.measure.library ) of
            ( Just _, Just _ ) ->
                case model.reports of
                    Loading ->
                        text ""

                    LoadingSlowly ->
                        text ""

                    Loaded reports ->
                        viewReportList onReportClick reports

                    Reloading _ ->
                        text ""

                    ReloadingSlowly _ ->
                        text ""

                    Failed _ ->
                        text "error"

            ( Nothing, _ ) ->
                missingUrlMessage

            ( _, Nothing ) ->
                missingLibraryMessage onLibraryAssoc
        ]


generateButton onMsg label =
    Button.secondary
        (Button.config |> Button.setOnClick (onMsg ClickedGenerate))
        label


viewReportList onReportClick reports =
    if List.isEmpty reports then
        div [] [ text "No reports available." ]

    else
        List.list List.config
            (List.map (viewReport onReportClick) reports)


viewReport : (Id -> msg) -> MeasureReport -> ListItem msg
viewReport onReportClick ({ status, date } as report) =
    ListItem.listItem
        (ListItem.config
            |> ListItem.setOnClick (onReportClick report.id)
        )
        report.id
        [ text (Maybe.withDefault "<unknown-date>" date)
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


viewErrorDialog error =
    Dialog.dialog
        (Dialog.config
            |> Dialog.setOpen (Nothing /= error)
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
                        [ h4 [ class "text-lg" ]
                            [ text (issueTitle issue.code) ]
                        , p [] [ text (Maybe.withDefault "" issue.diagnostics) ]
                        , h5 [ class "text-md" ]
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
    Button.primary
        (Button.config |> Button.setOnClick ClickedErrorDialogClose)
        "Ok"
