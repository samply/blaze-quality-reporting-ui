module Page.Library.List exposing
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
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Fhir.ValueSet.PublicationStatus as PublicationStatus exposing (PublicationStatus)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, title)
import HumanTime
import Json.Decode as Decode exposing (Decoder)
import Loading exposing (Status(..))
import Page.Library.List.Header as Header
import Page.Library.List.Query exposing (Query)
import Page.Library.List.Sort as Sort exposing (Sort)
import Route
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
    , libraryPage : Status LibraryPage
    , searchFieldFocused : Bool
    , query : Query
    }


type alias LibraryPage =
    { num : Int
    , list : List ( String, Result Decode.Error Library )
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
      , libraryPage = Loading
      , searchFieldFocused = False
      , query = query
      }
    , Cmd.batch
        [ headerCmd |> Cmd.map GotHeaderMsg
        , searchLibraries (Session.getBase session) query
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
    = ClickedLibrary Library
    | ClickedCreateLibrary
    | ClickedStatus PublicationStatus
    | ClickedFirst
    | ClickedNext Int
    | SearchLibraries String
    | Hit Events.Key
    | SearchFieldFocused (Result Dom.Error ())
    | SearchFieldBlurred (Result Dom.Error ())
    | CompletedLoadingInitialLibraryPage (Result FhirHttp.Error Bundle)
    | CompletedLoadingNextLibraryPage Int (Result FhirHttp.Error Bundle)
    | PassedSlowLoadingThreshold
    | CompletedCreatingLibrary (Result FhirHttp.Error Library)
    | GotCurrentTime Time.Posix
    | GotHeaderMsg Header.Msg
    | GotHeaderSortSelect Sort


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLibrary library ->
            ( model, pushLibraryUrl model library.id )

        ClickedCreateLibrary ->
            ( model, createLibrary (Session.getBase model.session) )

        ClickedStatus status ->
            updateQuery (setQueryStatus status) model

        ClickedFirst ->
            ( { model | libraryPage = Loading.markReloading model.libraryPage }
            , model.libraryPage
                |> Loading.andThen .firstUrl
                |> Maybe.map (loadPage CompletedLoadingInitialLibraryPage)
                |> Maybe.withDefault Cmd.none
            )

        ClickedNext oldPageNum ->
            ( { model | libraryPage = Loading.markReloading model.libraryPage }
            , model.libraryPage
                |> Loading.andThen .nextUrl
                |> Maybe.map (loadPage (CompletedLoadingNextLibraryPage oldPageNum))
                |> Maybe.withDefault Cmd.none
            )

        SearchLibraries title ->
            updateQuery (setQueryTitle title) model

        Hit Slash ->
            ( model
            , Task.attempt SearchFieldFocused (Dom.focus "search-field")
            )

        Hit Esc ->
            ( model
            , Task.attempt SearchFieldBlurred (Dom.blur "search-field")
            )

        SearchFieldFocused _ ->
            ( { model | searchFieldFocused = True }, Cmd.none )

        SearchFieldBlurred _ ->
            ( { model | searchFieldFocused = False }, Cmd.none )

        CompletedLoadingInitialLibraryPage (Ok bundle) ->
            ( { model | libraryPage = Loaded (decodeInitialLibraryPage bundle) }
            , Cmd.none
            )

        CompletedLoadingInitialLibraryPage (Err error) ->
            ( { model | libraryPage = Failed error }
            , Cmd.none
            )

        CompletedLoadingNextLibraryPage oldPageNum (Ok bundle) ->
            let
                firstUrl =
                    model.libraryPage |> Loading.andThen .firstUrl

                page =
                    decodeNextLibraryPage oldPageNum firstUrl bundle
            in
            ( { model | libraryPage = Loaded page }
            , Cmd.none
            )

        CompletedLoadingNextLibraryPage _ (Err error) ->
            ( { model | libraryPage = Failed error }
            , Cmd.none
            )

        PassedSlowLoadingThreshold ->
            ( { model
                | libraryPage = Loading.markLoadingSlowly model.libraryPage
              }
            , Cmd.none
            )

        CompletedCreatingLibrary (Ok library) ->
            ( model, pushLibraryUrl model library.id )

        CompletedCreatingLibrary (Err _) ->
            ( model, Cmd.none )

        GotCurrentTime currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )

        GotHeaderMsg headerMsg ->
            let
                ( header, headerCmd ) =
                    Header.update headerMsg model.header
            in
            ( { model | header = header }, headerCmd |> Cmd.map GotHeaderMsg )

        GotHeaderSortSelect sort ->
            { model | header = Header.closeSortDropdown model.header }
                |> updateQuery (setQuerySort sort)


updateQuery : (Query -> Query) -> Model -> ( Model, Cmd Msg )
updateQuery f model =
    let
        newQuery =
            f model.query

        ( newHeader, headerCmd ) =
            Header.updateQuery newQuery model.header
    in
    ( { model
        | header = newHeader
        , libraryPage = Loading.markReloading model.libraryPage
        , query = newQuery
      }
    , Cmd.batch
        [ searchLibraries (Session.getBase model.session) newQuery
        , headerCmd |> Cmd.map GotHeaderMsg
        ]
    )


pushLibraryUrl model id =
    Route.pushUrl (Session.toNavKey model.session) (Route.Library id)


searchLibraries : String -> Query -> Cmd Msg
searchLibraries base query =
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
        [ FhirHttp.searchType CompletedLoadingInitialLibraryPage
            base
            "Library"
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


decodeInitialLibraryPage : Bundle -> LibraryPage
decodeInitialLibraryPage bundle =
    { num = 0
    , list = List.indexedMap decodeLibraryList bundle.entry
    , firstUrl = Bundle.linkUrl "self" bundle
    , nextUrl = Bundle.linkUrl "next" bundle
    }


decodeNextLibraryPage : Int -> Maybe String -> Bundle -> LibraryPage
decodeNextLibraryPage oldNum firstUrl bundle =
    { num = oldNum + 1
    , list = List.indexedMap decodeLibraryList bundle.entry
    , firstUrl = firstUrl
    , nextUrl = Bundle.linkUrl "next" bundle
    }


decodeLibraryList : Int -> BundleEntry -> ( String, Result Decode.Error Library )
decodeLibraryList idx ({ fullUrl, resource } as entry) =
    ( fullUrl |> Maybe.withDefault (String.fromInt idx)
    , Entry.decodeResource Library.decoder entry
    )


createLibrary : String -> Cmd Msg
createLibrary base =
    let
        library =
            { id = ""
            , meta = Nothing
            , url = Nothing
            , version = Nothing
            , name = Nothing
            , title = Nothing
            , status = PublicationStatus.Draft
            , type_ = CodeableConcept.ofOneCoding (Library.type_ "logic-library")
            , subjectCodeableConcept = Nothing
            , subjectReference = Nothing
            , description = Nothing
            , content = []
            }
    in
    FhirHttp.create
        CompletedCreatingLibrary
        base
        "Library"
        Library.decoder
        (Library.encode library)



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
    { title = [ "Libraries" ]
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
                ++ libraryList
                    { timeZone = model.session.timeZone
                    , currentTime = model.currentTime
                    }
                    model.libraryPage
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
            |> TextField.setOnChange SearchLibraries
            |> TextField.setAttributes [ class "mr-2" ]
        )


createButton =
    Button.create
        (Button.config
            |> Button.setOnClick ClickedCreateLibrary
        )
        "New Library"


libraryList : TimeCtx -> Status LibraryPage -> List (Html Msg)
libraryList timeCtx statusLibraryPage =
    let
        libraryList2 libraryPage showPlaceholder =
            [ if List.isEmpty libraryPage.list then
                libraryListPlaceholder "No results matched your search."

              else
                List.list (List.config |> List.setTopBorder False)
                    (List.map (listItem timeCtx showPlaceholder) libraryPage.list)
            , pager libraryPage.num libraryPage.nextUrl
            ]
    in
    case statusLibraryPage of
        Loading ->
            [ libraryListPlaceholder "" ]

        LoadingSlowly ->
            [ libraryListPlaceholder "Loading..." ]

        Loaded libraryPage ->
            libraryList2 libraryPage False

        Reloading libraryPage ->
            libraryList2 libraryPage False

        ReloadingSlowly libraryPage ->
            libraryList2 libraryPage True

        Failed _ ->
            [ libraryListPlaceholder "Error while loading Libraries." ]


libraryListPlaceholder msg =
    div
        [ class "border border-gray-400 rounded-lg"
        , class "border-t-0 rounded-t-none"
        , class "p-16"
        , class "text-2xl text-center text-gray-800"
        , class "bg-white"
        ]
        [ text msg ]


listItem : TimeCtx -> Bool -> ( String, Result Decode.Error Library ) -> ListItem Msg
listItem timeCtx showPlaceholder ( id, result ) =
    case result of
        Ok library ->
            libraryListItem timeCtx showPlaceholder id library

        Err error ->
            errorListItem id error


libraryListItem : TimeCtx -> Bool -> String -> Library -> ListItem Msg
libraryListItem timeCtx showPlaceholder id library =
    ListItem.listItem
        (ListItem.config
            |> Util.applyIf (not showPlaceholder) (ListItem.setOnClick (ClickedLibrary library))
            |> ListItem.setAttributes [ class "text-gray-800" ]
        )
        id
        [ if showPlaceholder then
            ListItem.text []
                { primary =
                    [ span [ class "bg-gray-400 text-transparent leading-tight rounded" ] <|
                        libraryTitle library
                    ]
                , secondary =
                    [ span [ class "bg-gray-200 text-transparent leading-tight rounded" ] <|
                        libraryDetails timeCtx library
                    ]
                }

          else
            ListItem.text []
                { primary = libraryTitle library
                , secondary = libraryDetails timeCtx library
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


libraryTitle : Library -> List (Html msg)
libraryTitle { title, name, id } =
    List.filterMap identity [ title, name ]
        |> List.head
        |> Maybe.withDefault id
        |> Html.text
        |> List.singleton


libraryDetails : TimeCtx -> Library -> List (Html msg)
libraryDetails ({ timeZone } as timeCtx) { meta } =
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
