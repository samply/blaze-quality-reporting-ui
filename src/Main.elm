module Main exposing (Msg(..), main, update, view)

import Browser exposing (Document)
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
import Task
import Time



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


init : Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        flags =
            decodeValue flagsDecoder flagsValue
                |> Result.withDefault defaultFlags

        ( subModel, subCmd ) =
            MeasureList.init flags.session
    in
    ( { drawerOpen = True, page = MeasureList subModel }
    , Cmd.batch
        [ Cmd.map GotMeasureListMsg subCmd
        , Task.perform GotTimeZone Time.here
        ]
    )


flagsDecoder : Decoder Flags
flagsDecoder =
    let
        sessionDecoder =
            Decode.string
                |> Decode.map (decodeString Session.decoder)
                |> Decode.map (Result.withDefault Session.default)
    in
    succeed Flags
        |> optional "session" sessionDecoder Session.default


defaultFlags : Flags
defaultFlags =
    { session = Session.default }



-- UPDATE


type Msg
    = ClickedNavIcon
    | ClickedNavItem Page.NavItem
    | ClosedDrawer
    | GotTimeZone Time.Zone
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
        ( ClickedNavIcon, _ ) ->
            ( { model | drawerOpen = True }, Cmd.none )

        ( ClickedNavItem navItem, _ ) ->
            changeRoute (toSession model.page) (toRoute navItem) model

        ( ClosedDrawer, _ ) ->
            ( { model | drawerOpen = False }, Cmd.none )

        ( GotTimeZone timeZone, _ ) ->
            ( updateSession (\session -> { session | timeZone = timeZone })
                model
            , Cmd.none
            )

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


updateSession : (Session -> Session) -> Model -> Model
updateSession f model =
    case model.page of
        Redirect session ->
            { model | page = Redirect (f session) }

        NotFound session ->
            { model | page = NotFound (f session) }

        LibraryList libraryList ->
            { model
                | page = LibraryList (LibraryList.updateSession f libraryList)
            }

        Library library ->
            { model | page = Library (Library.updateSession f library) }

        MeasureList measureList ->
            { model
                | page = MeasureList (MeasureList.updateSession f measureList)
            }

        Measure measure ->
            { model | page = Measure (Measure.updateSession f measure) }

        MeasureReport measure ->
            { model
                | page = MeasureReport (MeasureReport.updateSession f measure)
            }

        Settings settings ->
            { model | page = Settings (Settings.updateSession f settings) }

        Help help ->
            { model | page = Help (Help.updateSession f help) }


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


changeRoute : Session -> Route -> Model -> ( Model, Cmd Msg )
changeRoute session route model =
    case route of
        Route.LibraryList ->
            let
                ( routeModel, routeCmd ) =
                    LibraryList.init session
            in
            ( { model | page = LibraryList routeModel }
            , Cmd.map GotLibraryListMsg routeCmd
            )

        Route.Library id ->
            let
                ( routeModel, routeCmd ) =
                    Library.init session id
            in
            ( { model | page = Library routeModel }
            , Cmd.map GotLibraryMsg routeCmd
            )

        Route.MeasureList ->
            let
                ( routeModel, routeCmd ) =
                    MeasureList.init session
            in
            ( { model | page = MeasureList routeModel }
            , Cmd.map GotMeasureListMsg routeCmd
            )

        Route.Measure id ->
            let
                ( routeModel, routeCmd ) =
                    Measure.init session id
            in
            ( { model | page = Measure routeModel }
            , Cmd.map GotMeasureMsg routeCmd
            )

        Route.MeasureReport id ->
            let
                ( routeModel, routeCmd ) =
                    MeasureReport.init session id
            in
            ( { model | page = MeasureReport routeModel }
            , Cmd.map GotMeasureReportMsg routeCmd
            )

        Route.Settings ->
            let
                ( routeModel, routeCmd ) =
                    Settings.init session
            in
            ( { model | page = Settings routeModel }
            , Cmd.map GotSettingsMsg routeCmd
            )

        Route.Help ->
            let
                ( routeModel, routeCmd ) =
                    Help.init session
            in
            ( { model | page = Help routeModel }
            , Cmd.map GotHelpMsg routeCmd
            )


updateWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg, Maybe Route )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd, maybeRoute ) =
    case maybeRoute of
        Just route ->
            changeRoute (toSession <| toModel subModel) route model

        Nothing ->
            ( { model | page = toModel subModel }
            , Cmd.map toMsg subCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { page } =
    case page of
        LibraryList libraryList ->
            LibraryList.subscriptions libraryList
                |> Sub.map GotLibraryListMsg

        MeasureList measureList ->
            MeasureList.subscriptions measureList
                |> Sub.map GotMeasureListMsg

        _ ->
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
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
