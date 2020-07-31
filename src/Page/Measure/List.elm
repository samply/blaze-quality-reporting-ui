module Page.Measure.List exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , updateSession
    , view
    )

import Browser.Dom as Dom
import Browser.Events
import Component.Button as Button
import Component.List as List
import Component.List.Item as ListItem exposing (ListItem)
import Component.TextField as TextField
import Events exposing (Key(..))
import Fhir.Bundle as Bundle exposing (Bundle)
import Fhir.Bundle.Entry as Entry exposing (BundleEntry)
import Fhir.CodeableConcept as CodeableConcept
import Fhir.Expression as Expression
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Fhir.ValueSet.PublicationStatus as PublicationStatus exposing (PublicationStatus)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, title)
import HumanTime
import Json.Decode as Decode exposing (Decoder)
import Loading exposing (Status(..))
import Page.Measure.List.Header as Header
import Page.Measure.List.Query exposing (Query)
import Page.Measure.List.Sort as Sort exposing (Sort)
import Route exposing (Route)
import Session exposing (Session)
import String.Extra
import Task
import Time
import Url.Builder as UrlBuilder
import Util exposing (TimeCtx)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    , currentTime : Time.Posix
    , measurePage : Status MeasurePage
    , searchFieldFocused : Bool
    , query : Query
    }


type alias MeasurePage =
    { num : Int
    , list : List ( String, Result Decode.Error Measure )
    , firstUrl : Maybe String
    , nextUrl : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        query =
            { status = PublicationStatus.Active
            , title = Nothing
            , sort = Sort.RecentlyUpdated
            }

        ( header, headerCmd ) =
            Header.init (Session.getBase session) query
    in
    ( { session = session
      , header = header
      , currentTime = Time.millisToPosix 0
      , measurePage = Loading
      , searchFieldFocused = False
      , query = query
      }
    , Cmd.batch
        [ headerCmd |> Cmd.map GotHeaderMsg
        , searchMeasures (Session.getBase session) query
        , Task.perform GotCurrentTime Time.now
        ]
    )


toSession : Model -> Session
toSession model =
    model.session


updateSession : (Session -> Session) -> Model -> Model
updateSession f model =
    { model | session = f model.session }


setQueryStatus : PublicationStatus -> Query -> Query
setQueryStatus status query =
    { query | status = status }


setQueryTitle : String -> Query -> Query
setQueryTitle title query =
    { query | title = String.Extra.nonBlank title }


setQuerySort : Sort -> Query -> Query
setQuerySort sort query =
    { query | sort = sort }



-- UPDATE


type Msg
    = ClickedMeasure Measure
    | ClickedNewMeasure
    | ClickedStatus PublicationStatus
    | ClickedFirst
    | ClickedNext Int
    | SearchMeasures String
    | Hit Events.Key
    | SearchFieldFocused (Result Dom.Error ())
    | SearchFieldBlurred (Result Dom.Error ())
    | CompletedLoadingInitialMeasurePage (Result FhirHttp.Error Bundle)
    | CompletedLoadingNextMeasurePage Int (Result FhirHttp.Error Bundle)
    | PassedSlowLoadingThreshold
    | CompletedCreatingMeasure (Result FhirHttp.Error Measure)
    | GotCurrentTime Time.Posix
    | GotHeaderMsg Header.Msg
    | GotHeaderSortSelect Sort


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Route )
update msg model =
    case msg of
        ClickedMeasure measure ->
            ( model, Cmd.none, Just (Route.Measure measure.id) )

        ClickedNewMeasure ->
            ( model, createMeasure (Session.getBase model.session), Nothing )

        ClickedStatus status ->
            updateQuery (setQueryStatus status) model

        ClickedFirst ->
            ( { model | measurePage = Loading.markReloading model.measurePage }
            , model.measurePage
                |> Loading.andThen .firstUrl
                |> Maybe.map (loadPage CompletedLoadingInitialMeasurePage)
                |> Maybe.withDefault Cmd.none
            , Nothing
            )

        ClickedNext oldPageNum ->
            ( { model | measurePage = Loading.markReloading model.measurePage }
            , model.measurePage
                |> Loading.andThen .nextUrl
                |> Maybe.map (loadPage (CompletedLoadingNextMeasurePage oldPageNum))
                |> Maybe.withDefault Cmd.none
            , Nothing
            )

        SearchMeasures title ->
            updateQuery (setQueryTitle title) model

        Hit Slash ->
            ( model
            , Task.attempt SearchFieldFocused (Dom.focus "search-field")
            , Nothing
            )

        Hit Esc ->
            ( model
            , Task.attempt SearchFieldBlurred (Dom.blur "search-field")
            , Nothing
            )

        SearchFieldFocused _ ->
            ( { model | searchFieldFocused = True }, Cmd.none, Nothing )

        SearchFieldBlurred _ ->
            ( { model | searchFieldFocused = False }, Cmd.none, Nothing )

        CompletedLoadingInitialMeasurePage (Ok bundle) ->
            ( { model | measurePage = Loaded (decodeInitialMeasurePage bundle) }
            , Cmd.none
            , Nothing
            )

        CompletedLoadingInitialMeasurePage (Err error) ->
            ( { model | measurePage = Failed error }
            , Cmd.none
            , Nothing
            )

        CompletedLoadingNextMeasurePage oldPageNum (Ok bundle) ->
            let
                firstUrl =
                    model.measurePage |> Loading.andThen .firstUrl

                page =
                    decodeNextMeasurePage oldPageNum firstUrl bundle
            in
            ( { model | measurePage = Loaded page }
            , Cmd.none
            , Nothing
            )

        CompletedLoadingNextMeasurePage _ (Err error) ->
            ( { model | measurePage = Failed error }
            , Cmd.none
            , Nothing
            )

        PassedSlowLoadingThreshold ->
            ( { model
                | measurePage = Loading.markLoadingSlowly model.measurePage
              }
            , Cmd.none
            , Nothing
            )

        CompletedCreatingMeasure (Ok measure) ->
            ( model
            , Cmd.none
            , Just (Route.Measure measure.id)
            )

        CompletedCreatingMeasure (Err _) ->
            ( model, Cmd.none, Nothing )

        GotCurrentTime currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none, Nothing )

        GotHeaderMsg headerMsg ->
            let
                ( header, headerCmd ) =
                    Header.update headerMsg model.header
            in
            ( { model | header = header }
            , headerCmd |> Cmd.map GotHeaderMsg
            , Nothing
            )

        GotHeaderSortSelect sort ->
            { model | header = Header.closeSortDropdown model.header }
                |> updateQuery (setQuerySort sort)


updateQuery : (Query -> Query) -> Model -> ( Model, Cmd Msg, Maybe Route )
updateQuery f model =
    let
        newQuery =
            f model.query

        ( newHeader, headerCmd ) =
            Header.updateQuery newQuery model.header
    in
    ( { model
        | header = newHeader
        , measurePage = Loading.markReloading model.measurePage
        , query = newQuery
      }
    , Cmd.batch
        [ searchMeasures (Session.getBase model.session) newQuery
        , headerCmd |> Cmd.map GotHeaderMsg
        ]
    , Nothing
    )


searchMeasures : String -> Query -> Cmd Msg
searchMeasures base query =
    let
        sortValue sort =
            case sort of
                Sort.RecentlyUpdated ->
                    "-_lastUpdated"

                Sort.LeastRecentlyUpdated ->
                    "_lastUpdated"

                Sort.Title ->
                    "title"
    in
    Cmd.batch
        [ FhirHttp.searchType CompletedLoadingInitialMeasurePage
            base
            "Measure"
            (List.filterMap identity
                [ Just (UrlBuilder.string "status" (PublicationStatus.toString query.status))
                , Maybe.map (UrlBuilder.string "title") query.title
                , Just (UrlBuilder.string "_sort" (sortValue query.sort))
                , Just (UrlBuilder.string "_count" "25")
                ]
            )
        , Loading.slowThreshold PassedSlowLoadingThreshold
        ]


loadPage : (Result FhirHttp.Error Bundle -> Msg) -> String -> Cmd Msg
loadPage msg url =
    Cmd.batch
        [ FhirHttp.loadBundle msg url
        , Loading.slowThreshold PassedSlowLoadingThreshold
        ]


decodeInitialMeasurePage : Bundle -> MeasurePage
decodeInitialMeasurePage bundle =
    { num = 0
    , list = List.indexedMap decodeMeasureList bundle.entry
    , firstUrl = Bundle.linkUrl "self" bundle
    , nextUrl = Bundle.linkUrl "next" bundle
    }


decodeNextMeasurePage : Int -> Maybe String -> Bundle -> MeasurePage
decodeNextMeasurePage oldNum firstUrl bundle =
    { num = oldNum + 1
    , list = List.indexedMap decodeMeasureList bundle.entry
    , firstUrl = firstUrl
    , nextUrl = Bundle.linkUrl "next" bundle
    }


decodeMeasureList : Int -> BundleEntry -> ( String, Result Decode.Error Measure )
decodeMeasureList idx ({ fullUrl, resource } as entry) =
    ( fullUrl |> Maybe.withDefault (String.fromInt idx)
    , Entry.decodeResource Measure.decoder entry
    )


createMeasure : String -> Cmd Msg
createMeasure base =
    let
        measure =
            { id = ""
            , meta = Nothing
            , url = Nothing
            , version = Nothing
            , name = Nothing
            , title = Nothing
            , subtitle = Nothing
            , status = PublicationStatus.Draft
            , subject =
                Just
                    { coding =
                        [ { system = Just "http://hl7.org/fhir/resource-types"
                          , version = Nothing
                          , code = Just "Patient"
                          }
                        ]
                    , text = Nothing
                    }
            , description = Nothing
            , library = []
            , scoring =
                Just (CodeableConcept.ofOneCoding (Measure.scoring "cohort"))
            , group =
                [ { code = Nothing
                  , description = Nothing
                  , population =
                        [ { code =
                                Just
                                    (CodeableConcept.ofOneCoding
                                        (Measure.populationType
                                            "initial-population"
                                        )
                                    )
                          , description = Nothing
                          , criteria =
                                Just (Expression.cql (Just "InInitialPopulation"))
                          }
                        ]
                  , stratifier = []
                  }
                ]
            }
    in
    FhirHttp.create
        CompletedCreatingMeasure
        base
        "Measure"
        Measure.decoder
        (Measure.encode measure)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { searchFieldFocused } =
    if not searchFieldFocused then
        Sub.batch
            [ Browser.Events.onKeyUp (Events.keyDecoder Hit)
            , Time.every (60 * 1000) GotCurrentTime
            ]

    else
        Time.every (60 * 1000) GotCurrentTime



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    { title = [ "Measures" ]
    , content =
        div
            [ class "mt-16 ml-48 p-6 flex-grow bg-gray-100" ]
            ([ topPanel
             , Header.view
                { onMsg = GotHeaderMsg
                , onStatusClick = ClickedStatus
                , onSortSelect = GotHeaderSortSelect
                }
                model.header
             ]
                ++ measureList
                    { timeZone = model.session.timeZone
                    , currentTime = model.currentTime
                    }
                    model.measurePage
            )
    }


topPanel =
    div [ class "flex justify-between mb-2" ]
        [ searchField
        , createButton
        ]


searchField =
    TextField.outlined
        (TextField.config
            |> TextField.setId (Just "search-field")
            |> TextField.setPlaceholder (Just "Search (Press \"/\" to focus)")
            |> TextField.setOnChange SearchMeasures
            |> TextField.setAttributes [ class "mr-2" ]
        )


createButton =
    Button.create
        (Button.config
            |> Button.setOnClick ClickedNewMeasure
        )
        "New Measure"


measureList : TimeCtx -> Status MeasurePage -> List (Html Msg)
measureList timeCtx statusMeasurePage =
    let
        measureList2 measurePage showPlaceholder =
            [ if List.isEmpty measurePage.list then
                measureListPlaceholder "No results matched your search."

              else
                List.list (List.config |> List.setTopBorder False)
                    (List.map (listItem timeCtx showPlaceholder) measurePage.list)
            , pager measurePage.num measurePage.nextUrl
            ]
    in
    case statusMeasurePage of
        Loading ->
            [ measureListPlaceholder "" ]

        LoadingSlowly ->
            [ measureListPlaceholder "Loading..." ]

        Loaded measurePage ->
            measureList2 measurePage False

        Reloading measurePage ->
            measureList2 measurePage False

        ReloadingSlowly measurePage ->
            measureList2 measurePage True

        Failed _ ->
            [ measureListPlaceholder "Error while loading Measures." ]


measureListPlaceholder msg =
    div
        [ class "border border-gray-400 rounded-lg"
        , class "border-t-0 rounded-t-none"
        , class "p-16"
        , class "text-2xl text-center text-gray-800"
        , class "bg-white"
        ]
        [ text msg ]


listItem : TimeCtx -> Bool -> ( String, Result Decode.Error Measure ) -> ListItem Msg
listItem timeCtx showPlaceholder ( id, result ) =
    case result of
        Ok measure ->
            measureListItem timeCtx showPlaceholder id measure

        Err error ->
            errorListItem id error


measureListItem : TimeCtx -> Bool -> String -> Measure -> ListItem Msg
measureListItem timeCtx showPlaceholder id measure =
    ListItem.listItem
        (ListItem.config
            |> Util.applyIf (not showPlaceholder) (ListItem.setOnClick (ClickedMeasure measure))
            |> ListItem.setAttributes [ class "text-gray-800" ]
        )
        id
        [ if showPlaceholder then
            ListItem.text []
                { primary =
                    [ span [ class "bg-gray-400 text-transparent leading-tight rounded" ] <|
                        measureTitle measure
                    ]
                , secondary =
                    [ span [ class "bg-gray-200 text-transparent leading-tight rounded" ] <|
                        measureDetails timeCtx measure
                    ]
                }

          else
            ListItem.text []
                { primary = measureTitle measure
                , secondary = measureDetails timeCtx measure
                }
        ]


errorListItem : String -> Decode.Error -> ListItem Msg
errorListItem id error =
    ListItem.listItem
        (ListItem.config
            |> ListItem.setAttributes [ class "text-gray-800" ]
        )
        id
        [ text <| Decode.errorToString error ]


measureTitle : Measure -> List (Html msg)
measureTitle { title, name, id } =
    List.filterMap identity [ title, name ]
        |> List.head
        |> Maybe.withDefault id
        |> Html.text
        |> List.singleton


measureDetails : TimeCtx -> Measure -> List (Html msg)
measureDetails ({ timeZone } as timeCtx) { meta } =
    case meta |> Maybe.andThen .lastUpdated of
        Just lastUpdated ->
            [ span [ title <| HumanTime.format timeZone lastUpdated ]
                [ text <| "updated " ++ formatTimeAgo timeCtx lastUpdated ]
            ]

        Nothing ->
            [ text <| "unknown updated time" ]


formatTimeAgo : TimeCtx -> Time.Posix -> String
formatTimeAgo { timeZone, currentTime } time =
    HumanTime.formatTimeAgo timeZone currentTime time


pager : Int -> Maybe String -> Html Msg
pager oldPageNum nextUrl =
    div [ class "flex justify-center space-x-4 mt-4" ]
        [ if oldPageNum == 0 then
            Button.primary
                (Button.config
                    |> Button.setDisabled True
                    |> Button.setOnClick ClickedFirst
                )
                "1"

          else
            Button.secondary
                (Button.config
                    |> Button.setOnClick ClickedFirst
                )
                "1"
        , Button.text
            (Button.config
                |> Button.setDisabled (nextUrl == Nothing)
                |> Button.setOnClick (ClickedNext oldPageNum)
            )
            "Next ❯"
        ]
