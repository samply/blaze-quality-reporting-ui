module Page.Measure.ReportPanel exposing (Model, Msg, init, update, view)

import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.MeasureReport as MeasureReport exposing (MeasureReport, Status(..))
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (decodeValue)
import Loading
import Material.Button exposing (buttonConfig, outlinedButton, textButton)
import Material.Icon exposing (icon, iconConfig)
import Material.LayoutGrid exposing (layoutGridCell, span12)
import Material.List
    exposing
        ( ListItem
        , list
        , listConfig
        , listGroup
        , listGroupSubheader
        , listItem
        , listItemConfig
        , listItemGraphic
        , listItemMeta
        , listItemPrimaryText
        , listItemSecondaryText
        , listItemText
        )
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { base : String
    , measureId : Id
    , reports : Loading.Status (List MeasureReport)
    }


init : String -> Id -> ( Model, Cmd Msg )
init base measureId =
    ( { base = base
      , measureId = measureId
      , reports = Loading.Loading
      }
    , loadReports base measureId
    )



-- UPDATE


type Msg
    = ClickedGenerate
    | CompletedLoadReports (Result Http.Error Bundle)
    | CompletedGenerateReport (Result Http.Error MeasureReport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGenerate ->
            ( model
            , generateReport model.base model.measureId
            )

        CompletedLoadReports (Ok bundle) ->
            ( { model | reports = Loading.Loaded (decodeReports bundle) }
            , Cmd.none
            )

        CompletedLoadReports (Err _) ->
            ( { model | reports = Loading.Failed }
            , Cmd.none
            )

        CompletedGenerateReport (Ok report) ->
            ( addReport report model
            , Cmd.none
            )

        CompletedGenerateReport (Err _) ->
            ( { model | reports = Loading.Failed }
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

        Loading.Failed ->
            model


generateReport base measureId =
    Http.post
        { url =
            UrlBuilder.crossOrigin base
                [ "Measure", measureId, "$evaluate-measure" ]
                [ UrlBuilder.string "periodStart" "1900"
                , UrlBuilder.string "periodEnd" "2100"
                ]
        , body = Http.emptyBody
        , expect = Http.expectJson CompletedGenerateReport MeasureReport.decoder
        }


loadReports base measureId =
    FhirHttp.searchType CompletedLoadReports
        base
        "MeasureReport"
        [ UrlBuilder.string "measure" measureId ]


decodeReports : Bundle -> List MeasureReport
decodeReports { entry } =
    List.filterMap
        (.resource >> decodeValue MeasureReport.decoder >> Result.toMaybe)
        entry



-- VIEW


type alias Config msg =
    { ready : Bool
    , onLibraryAssoc : msg
    , onMsg : Msg -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    layoutGridCell [ span12, class "measure-report-panel" ]
        [ h3
            [ class "measure-report-panel__title"
            , class "mdc-typography--headline5"
            ]
            [ text "Reports" ]
        , case model.reports of
            Loading.Loaded reports ->
                if List.isEmpty reports then
                    emptyListPlaceholder config

                else
                    viewReportList reports

            Loading.Loading ->
                text ""

            Loading.LoadingSlowly ->
                text ""

            Loading.Failed ->
                text "error"
        ]


emptyListPlaceholder { ready, onLibraryAssoc, onMsg } =
    div [ class "measure-report-panel__empty-placeholder" ] <|
        if ready then
            [ outlinedButton
                { buttonConfig | onClick = Just (onMsg ClickedGenerate) }
                "Generate First Report"
            ]

        else
            [ text "Please"
            , textButton
                { buttonConfig | onClick = Just onLibraryAssoc }
                "associate"
            , text "a library before generating a report."
            ]


viewReportList reports =
    list listConfig <|
        List.map viewReport reports


viewReport { status, date } =
    listItem listItemConfig
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
