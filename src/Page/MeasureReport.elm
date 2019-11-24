module Page.MeasureReport exposing (Model, Msg, init, toSession, update, view)

import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Http as FhirHttp
import Fhir.MeasureReport as MeasureReport exposing (MeasureReport)
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html, div, h3, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Loading exposing (Status(..))
import Material.List exposing (ListItem, list, listConfig, listItem, listItemConfig)
import NaturalOrdering
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , report : Status MeasureReport
    }


init : Session -> Id -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , report = Loading
      }
    , loadReport (Session.getBase session) id
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = CompletedLoadReport (Result FhirHttp.Error MeasureReport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedLoadReport (Ok report) ->
            ( { model | report = Loaded report }, Cmd.none )

        CompletedLoadReport (Err error) ->
            ( { model | report = Failed error }, Cmd.none )


loadReport base id =
    FhirHttp.read CompletedLoadReport
        base
        "MeasureReport"
        id
        MeasureReport.decoder



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    case model.report of
        Loaded report ->
            { title = [ "MeasureReport" ]
            , content =
                div [ class "main-content measure-report-page" ]
                    [ viewReport report ]
            }

        _ ->
            { title = [ "MeasureReport" ]
            , content =
                div [ class "main-content measure-report-page" ]
                    [ text "" ]
            }


viewReport : MeasureReport -> Html Msg
viewReport report =
    div [ class "measure-report" ] <|
        List.indexedMap viewGroup report.group


viewGroup : Int -> MeasureReport.Group -> Html Msg
viewGroup groupIdx { population, stratifier } =
    div [ class "measure-report-group" ]
        [ viewPopulationPanel groupIdx population
        , viewStratifierPanel groupIdx stratifier
        ]


viewPopulationPanel : Int -> List MeasureReport.Population -> Html Msg
viewPopulationPanel groupIdx populations =
    div [ class "measure-report-population-panel" ]
        [ h3 [ class "mdc-typography--headline5" ] [ text "Populations" ]
        , if List.isEmpty populations then
            p [] [ text "No populations" ]

          else
            list { listConfig | nonInteractive = True }
                (List.indexedMap (viewPopulation groupIdx) populations)
        ]


viewPopulation : Int -> Int -> MeasureReport.Population -> ListItem Msg
viewPopulation groupIdx populationIdx { code, count } =
    listItem
        listItemConfig
        [ text (populationType populationIdx code ++ ": " ++ countToString count)
        ]


populationType : Int -> Maybe CodeableConcept -> String
populationType populationIdx code =
    code
        |> Maybe.andThen (.coding >> List.head)
        |> Maybe.andThen .code
        |> Maybe.withDefault
            ("Population " ++ String.fromInt (populationIdx + 1))


viewStratifierPanel : Int -> List MeasureReport.Stratifier -> Html Msg
viewStratifierPanel groupIdx stratifiers =
    div [ class "measure-report-stratifier-panel" ]
        [ h3 [ class "mdc-typography--headline5" ] [ text "Stratifiers" ]
        , if List.isEmpty stratifiers then
            p [] [ text "No stratifiers" ]

          else
            div []
                (List.indexedMap (viewStratifier groupIdx) stratifiers)
        ]


viewStratifier : Int -> Int -> MeasureReport.Stratifier -> Html Msg
viewStratifier groupIdx stratifierIdx { code, stratum } =
    div [ class "measure-report-stratifier" ]
        [ div [ class "measure-report-stratifier__title" ]
            [ h3 [ class "mdc-typography--headline6" ]
                [ text (stratifierTitle stratifierIdx code) ]
            ]
        , div [ class "mdc-data-table" ]
            [ table [ class "mdc-data-table__table" ]
                [ thead []
                    [ tr [ class "mdc-data-table__header-row" ]
                        [ th [ class "mdc-data-table__header-cell" ]
                            [ text "Value" ]
                        , th
                            [ class "mdc-data-table__header-cell"
                            , class "mdc-data-table__header-cell--numeric"
                            ]
                            [ text "Count" ]
                        ]
                    ]
                , tbody [ class "mdc-data-table_content" ]
                    (List.map viewStratum
                        (List.sortWith
                            (NaturalOrdering.compareOn
                                (.value
                                    >> Maybe.andThen .text
                                    >> Maybe.withDefault "<unknown>"
                                )
                            )
                            stratum
                        )
                    )
                ]
            ]
        ]


stratifierTitle : Int -> List CodeableConcept -> String
stratifierTitle stratifierIdx code =
    code
        |> List.head
        |> Maybe.andThen .text
        |> Maybe.withDefault
            ("Stratifier " ++ String.fromInt (stratifierIdx + 1))


viewStratum : MeasureReport.Stratum -> Html Msg
viewStratum stratum =
    let
        value =
            stratum.value
                |> Maybe.andThen .text
                |> Maybe.withDefault "<unknown>"

        countCell population =
            td
                [ class "mdc-data-table__cell"
                , class "mdc-data-table__cell--numeric"
                ]
                [ population.count |> countToString |> text ]
    in
    tr [ class "mdc-data-table__row" ]
        (td [ class "mdc-data-table__cell" ] [ text value ]
            :: List.map countCell stratum.population
        )


countToString : Maybe Int -> String
countToString count =
    count
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "<unknown>"


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