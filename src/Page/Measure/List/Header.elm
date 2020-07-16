module Page.Measure.List.Header exposing
    ( Model
    , Msg
    , closeSortDropdown
    , init
    , update
    , updateQuery
    , view
    )

import Component.Dropdown as Dropdown
import Component.ToggleButton as ToggleButton
import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.ValueSet.PublicationStatus as PublicationStatus exposing (PublicationStatus)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Loading exposing (Status(..))
import Page.Measure.List.Query exposing (Query)
import Page.Measure.List.Sort as Sort exposing (Sort)
import Task
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { base : String
    , query : Query
    , activeCount : Status (Maybe Int)
    , draftCount : Status (Maybe Int)
    , retiredCount : Status (Maybe Int)
    , sortDropdown : Dropdown.Model
    }


init : String -> Query -> ( Model, Cmd Msg )
init base query =
    ( { base = base
      , query = query
      , activeCount = Loading
      , draftCount = Loading
      , retiredCount = Loading
      , sortDropdown = Dropdown.init
      }
    , countStatuses base query
    )



-- UPDATE


type Msg
    = CompletedLoadingStatusCounts PublicationStatus (Result FhirHttp.Error Bundle)
    | PassedSlowLoadingThreshold
    | GotSortMsg Dropdown.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedLoadingStatusCounts status (Ok bundle) ->
            case status of
                PublicationStatus.Active ->
                    ( { model | activeCount = Loaded bundle.total }, Cmd.none )

                PublicationStatus.Draft ->
                    ( { model | draftCount = Loaded bundle.total }, Cmd.none )

                PublicationStatus.Retired ->
                    ( { model | retiredCount = Loaded bundle.total }, Cmd.none )

                PublicationStatus.Unknown ->
                    ( model, Cmd.none )

        CompletedLoadingStatusCounts status (Err error) ->
            case status of
                PublicationStatus.Active ->
                    ( { model | activeCount = Failed error }, Cmd.none )

                PublicationStatus.Draft ->
                    ( { model | draftCount = Failed error }, Cmd.none )

                PublicationStatus.Retired ->
                    ( { model | retiredCount = Failed error }, Cmd.none )

                PublicationStatus.Unknown ->
                    ( model, Cmd.none )

        PassedSlowLoadingThreshold ->
            ( { model
                | activeCount = Loading.markLoadingSlowly model.activeCount
                , draftCount = Loading.markLoadingSlowly model.draftCount
                , retiredCount = Loading.markLoadingSlowly model.retiredCount
              }
            , Cmd.none
            )

        GotSortMsg sortMsg ->
            ( { model | sortDropdown = Dropdown.update sortMsg model.sortDropdown }
            , Cmd.none
            )


closeSortDropdown : Model -> Model
closeSortDropdown model =
    { model | sortDropdown = Dropdown.close model.sortDropdown }


updateQuery : Query -> Model -> ( Model, Cmd Msg )
updateQuery query model =
    if shouldRecalculateCounts query model.query then
        ( { model
            | query = query
            , activeCount = Loading.markReloading model.activeCount
            , draftCount = Loading.markReloading model.draftCount
            , retiredCount = Loading.markReloading model.retiredCount
          }
        , countStatuses model.base query
        )

    else
        ( { model | query = query }, Cmd.none )


shouldRecalculateCounts : Query -> Query -> Bool
shouldRecalculateCounts new old =
    new.title /= old.title


countStatuses : String -> Query -> Cmd Msg
countStatuses base query =
    Cmd.batch
        [ countStatus base PublicationStatus.Active query
        , countStatus base PublicationStatus.Draft query
        , countStatus base PublicationStatus.Retired query
        , Loading.slowThreshold PassedSlowLoadingThreshold
        ]


countStatus : String -> PublicationStatus -> Query -> Cmd Msg
countStatus base status query =
    FhirHttp.searchType (CompletedLoadingStatusCounts status)
        base
        "Measure"
        (List.filterMap identity
            [ Just (UrlBuilder.string "status" (PublicationStatus.toString status))
            , Maybe.map (UrlBuilder.string "title") query.title
            , Just (UrlBuilder.string "_summary" "count")
            ]
        )



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onStatusClick : PublicationStatus -> msg
    , onSortSelect : Sort -> msg
    }


view : Config msg -> Model -> Html msg
view { onMsg, onStatusClick, onSortSelect } model =
    let
        allCountsAvailable =
            Loading.hasData model.activeCount
                && Loading.hasData model.draftCount
                && Loading.hasData model.retiredCount
    in
    div
        [ class "flex justify-between"
        , class "border border-gray-400 rounded-lg rounded-b-none"
        , class "px-4 py-2"
        , class "bg-gray-300"
        ]
        [ div [ class "space-x-4" ]
            [ ToggleButton.text
                (ToggleButton.config
                    |> ToggleButton.setOn (PublicationStatus.Active == model.query.status)
                    |> ToggleButton.setOnClick (onStatusClick PublicationStatus.Active)
                )
                (buttonLabel allCountsAvailable model .activeCount "Active")
            , ToggleButton.text
                (ToggleButton.config
                    |> ToggleButton.setOn (PublicationStatus.Draft == model.query.status)
                    |> ToggleButton.setOnClick (onStatusClick PublicationStatus.Draft)
                )
                (buttonLabel allCountsAvailable model .draftCount "Draft")
            , ToggleButton.text
                (ToggleButton.config
                    |> ToggleButton.setOn (PublicationStatus.Retired == model.query.status)
                    |> ToggleButton.setOnClick (onStatusClick PublicationStatus.Retired)
                )
                (buttonLabel allCountsAvailable model .retiredCount "Retired")
            ]
        , div [ class "space-x-4" ]
            [ Dropdown.view
                (Dropdown.config (GotSortMsg >> onMsg) onSortSelect)
                "Sort"
                model.query.sort
                [ ( Sort.RecentlyUpdated, "Recently updated" )
                , ( Sort.LeastRecentlyUpdated, "Least recently updated" )
                , ( Sort.Title, "Title" )
                ]
                model.sortDropdown
            ]
        ]


buttonLabel : Bool -> Model -> (Model -> Status (Maybe Int)) -> String -> List (Html msg)
buttonLabel allCountsAvailable model accessor label =
    let
        buttonCountLabel count =
            [ span [] [ text <| String.fromInt count ]
            , span [] [ text <| " " ++ label ]
            ]

        fadedCountLabel count =
            [ span [ class "text-transparent" ] [ text <| String.fromInt count ]
            , span [] [ text <| " " ++ label ]
            ]
    in
    if allCountsAvailable then
        case accessor model of
            Loaded (Just count) ->
                buttonCountLabel count

            Reloading (Just count) ->
                buttonCountLabel count

            ReloadingSlowly (Just count) ->
                fadedCountLabel count

            _ ->
                [ text label ]

    else
        [ text label ]
