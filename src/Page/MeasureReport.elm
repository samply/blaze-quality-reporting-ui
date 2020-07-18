module Page.MeasureReport exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , updateSession
    , view
    )

import Component.DataTable as DataTable
import Component.List as List
import Component.List.Item as ListItem exposing (ListItem)
import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Http as FhirHttp
import Fhir.MeasureReport as MeasureReport exposing (MeasureReport)
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (class)
import Loading exposing (Status(..))
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


updateSession : (Session -> Session) -> Model -> Model
updateSession f model =
    { model | session = f model.session }



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
    { title = [ "MeasureReport" ]
    , content =
        div [ class "mt-16 ml-48 p-6 flex-grow bg-gray-100" ] <|
            viewReport model.report
    }


viewReport : Status MeasureReport -> List (Html Msg)
viewReport data =
    case data of
        Loading ->
            []

        LoadingSlowly ->
            []

        Loaded loadedReport ->
            [ viewLoadedReport loadedReport
            ]

        Reloading _ ->
            []

        ReloadingSlowly _ ->
            []

        Failed error ->
            [ text "" ]


viewLoadedReport : MeasureReport -> Html Msg
viewLoadedReport report =
    div [ class "" ] <|
        List.indexedMap viewGroup report.group


viewGroup : Int -> MeasureReport.Group -> Html Msg
viewGroup groupIdx { population, stratifier } =
    div [ class "" ]
        [ viewPopulationPanel groupIdx population
        , viewStratifierPanel groupIdx stratifier
        ]


viewPopulationPanel : Int -> List MeasureReport.Population -> Html Msg
viewPopulationPanel groupIdx populations =
    div [ class "mb-4" ]
        [ h3 [ class "text-lg mb-2" ] [ text "Populations" ]
        , if List.isEmpty populations then
            p [] [ text "No populations" ]

          else
            List.list (List.config |> List.setNonInteractive True) <|
                List.indexedMap (viewPopulation groupIdx) populations
        ]


viewPopulation : Int -> Int -> MeasureReport.Population -> ListItem Msg
viewPopulation groupIdx populationIdx { code, count } =
    ListItem.listItem
        ListItem.config
        (String.fromInt populationIdx)
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
    div [ class "mb-4" ]
        [ h3 [ class "text-lg mb-2" ] [ text "Stratifiers" ]
        , if List.isEmpty stratifiers then
            p [] [ text "No stratifiers" ]

          else
            div []
                (List.indexedMap (viewStratifier groupIdx) stratifiers)
        ]


viewStratifier : Int -> Int -> MeasureReport.Stratifier -> Html Msg
viewStratifier groupIdx stratifierIdx stratifier =
    let
        codeTexts =
            List.filterMap .text stratifier.code

        headerCell codeText =
            DataTable.cell [] [ text codeText ]
    in
    div [ class "" ]
        [ div [ class "" ]
            [ h3 [ class "text-md mb-2" ]
                [ text (stratifierTitle stratifier.code) ]
            ]
        , DataTable.dataTable
            (DataTable.config |> DataTable.setAttributes [ class "mb-4" ])
            { thead =
                [ DataTable.row [] <|
                    List.map headerCell codeTexts
                        ++ [ DataTable.numericCell [] [ text "Count" ] ]
                ]
            , tbody =
                List.map (viewStratum codeTexts)
                    (List.sortWith
                        (NaturalOrdering.compareOn
                            (\stratum ->
                                codeTexts
                                    |> List.map (stratumValue stratum)
                                    |> String.join " "
                            )
                        )
                        stratifier.stratum
                    )
            }
        ]


stratifierTitle : List CodeableConcept -> String
stratifierTitle code =
    code
        |> List.filterMap .text
        |> String.join ", "


stratumValue : MeasureReport.Stratum -> String -> String
stratumValue stratum codeText =
    if List.isEmpty stratum.component then
        stratum.value
            |> Maybe.andThen .text
            |> Maybe.withDefault "<unknown>"

    else
        stratum.component
            |> List.filter (.code >> .text >> (==) (Just codeText))
            |> List.head
            |> Maybe.andThen (.value >> .text)
            |> Maybe.withDefault "<unknown>"


viewStratum : List String -> MeasureReport.Stratum -> DataTable.Row Msg
viewStratum codeTexts stratum =
    let
        valueCell codeText =
            DataTable.cell []
                [ text (stratumValue stratum codeText) ]

        countCell population =
            DataTable.numericCell []
                [ population.count |> countToString |> text ]
    in
    DataTable.row []
        (List.map valueCell codeTexts
            ++ List.map countCell stratum.population
        )


countToString : Maybe Int -> String
countToString count =
    count
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "<unknown>"
