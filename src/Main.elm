module Main exposing (Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, succeed)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode exposing (Value)
import Page
import Page.Blank as Blank
import Page.Help as Help
import Page.Library as Library
import Page.Library.List as LibraryList
import Page.Measure as Measure
import Page.Measure.List as MeasureList
import Page.MeasureReport as MeasureReport
import Page.NotFound as NotFound
import Page.Settings as Settings
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



-- Model


type alias Model =
    { drawerOpen : Bool
    , page : Page
    }


type Page
    = Redirect Session
    | NotFound Session
    | LibraryList LibraryList.Model
    | Library Library.Model
    | MeasureList MeasureList.Model
    | Measure Measure.Model
    | MeasureReport MeasureReport.Model
    | Settings Settings.Model
    | Help Help.Model


type alias Flags =
    { session : Session }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url navKey =
    let
        flags =
            decodeValue (flagsDecoder navKey) flagsValue
                |> Result.withDefault (defaultFlags navKey)
    in
    changeRouteTo (Route.fromUrl url)
        { drawerOpen = False
        , page = Redirect flags.session
        }


flagsDecoder : Nav.Key -> Decoder Flags
flagsDecoder navKey =
    let
        sessionDecoder =
            Decode.string
                |> Decode.map (decodeString (Session.decoder navKey))
                |> Decode.map (Result.withDefault (Session.default navKey))
    in
    succeed Flags
        |> optional "session" sessionDecoder (Session.default navKey)


defaultFlags : Nav.Key -> Flags
defaultFlags navKey =
    { session = Session.default navKey }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | ClickedNavIcon
    | ClickedNavItem Page.NavItem
    | ClosedDrawer
    | GotLibraryListMsg LibraryList.Msg
    | GotLibraryMsg Library.Msg
    | GotMeasureListMsg MeasureList.Msg
    | GotMeasureMsg Measure.Msg
    | GotMeasureReportMsg MeasureReport.Msg
    | GotSettingsMsg Settings.Msg
    | GotHelpMsg Help.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl
                                (Session.toNavKey (toSession model.page))
                                (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ClickedNavIcon, _ ) ->
            ( { model | drawerOpen = True }, Cmd.none )

        ( ClickedNavItem navItem, _ ) ->
            ( { model | drawerOpen = False }
            , Route.pushUrl (Session.toNavKey (toSession model.page)) (toRoute navItem)
            )

        ( ClosedDrawer, _ ) ->
            ( { model | drawerOpen = False }, Cmd.none )

        ( GotLibraryListMsg subMsg, LibraryList libraryList ) ->
            LibraryList.update subMsg libraryList
                |> updateWith LibraryList GotLibraryListMsg model

        ( GotLibraryMsg subMsg, Library library ) ->
            Library.update subMsg library
                |> updateWith Library GotLibraryMsg model

        ( GotMeasureListMsg subMsg, MeasureList measureList ) ->
            MeasureList.update subMsg measureList
                |> updateWith MeasureList GotMeasureListMsg model

        ( GotMeasureMsg subMsg, Measure measure ) ->
            Measure.update subMsg measure
                |> updateWith Measure GotMeasureMsg model

        ( GotMeasureReportMsg subMsg, MeasureReport measureReport ) ->
            MeasureReport.update subMsg measureReport
                |> updateWith MeasureReport GotMeasureReportMsg model

        ( GotSettingsMsg subMsg, Settings settings ) ->
            Settings.update subMsg settings
                |> updateWith Settings GotSettingsMsg model

        ( GotHelpMsg subMsg, Help help ) ->
            Help.update subMsg help
                |> updateWith Help GotHelpMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


toSession : Page -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        LibraryList libraryList ->
            LibraryList.toSession libraryList

        Library library ->
            Library.toSession library

        MeasureList measureList ->
            MeasureList.toSession measureList

        Measure measure ->
            Measure.toSession measure

        MeasureReport measure ->
            MeasureReport.toSession measure

        Settings settings ->
            Settings.toSession settings

        Help help ->
            Help.toSession help


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model.page
    in
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound session }, Cmd.none )

        Just Route.LibraryList ->
            LibraryList.init session
                |> updateWith LibraryList GotLibraryListMsg model

        Just (Route.Library id) ->
            Library.init session id
                |> updateWith Library GotLibraryMsg model

        Just Route.MeasureList ->
            MeasureList.init session
                |> updateWith MeasureList GotMeasureListMsg model

        Just (Route.Measure id) ->
            Measure.init session id
                |> updateWith Measure GotMeasureMsg model

        Just (Route.MeasureReport id) ->
            MeasureReport.init session id
                |> updateWith MeasureReport GotMeasureReportMsg model

        Just Route.Settings ->
            Settings.init session
                |> updateWith Settings GotSettingsMsg model

        Just Route.Help ->
            Help.init session
                |> updateWith Help GotHelpMsg model


toRoute : Page.NavItem -> Route
toRoute navItem =
    case navItem of
        Page.Libraries ->
            Route.LibraryList

        Page.Measures ->
            Route.MeasureList

        Page.Settings ->
            Route.Settings

        Page.Help ->
            Route.Help


updateWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        pageViewConfig =
            { onDrawerClose = ClosedDrawer
            , onNavIconClick = ClickedNavIcon
            , onNavItemClick = ClickedNavItem
            }

        viewPage toMsg config =
            let
                { title, body } =
                    Page.view
                        toMsg
                        pageViewConfig
                        (toSession model.page)
                        config
            in
            { title = title
            , body = body
            }
    in
    case model.page of
        Redirect _ ->
            Page.view
                identity
                pageViewConfig
                (toSession model.page)
                Blank.view

        NotFound _ ->
            Page.view
                identity
                pageViewConfig
                (toSession model.page)
                NotFound.view

        LibraryList libraryList ->
            viewPage GotLibraryListMsg (LibraryList.view libraryList)

        Library library ->
            viewPage GotLibraryMsg (Library.view library)

        MeasureList measureList ->
            viewPage GotMeasureListMsg (MeasureList.view measureList)

        Measure measure ->
            viewPage GotMeasureMsg (Measure.view measure)

        MeasureReport measure ->
            viewPage GotMeasureReportMsg (MeasureReport.view measure)

        Settings settings ->
            viewPage GotSettingsMsg (Settings.view settings)

        Help help ->
            viewPage GotHelpMsg (Help.view help)



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
