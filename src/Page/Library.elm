module Page.Library exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , updateSession
    , view
    )

import Component.Error as Error
import Component.Header as Header
import Fhir.Attachment exposing (Attachment)
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Fhir.PrimitiveTypes exposing (Id)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import List.Zipper as Zipper
import Loading exposing (Status(..))
import Maybe.Extra as MaybeExtra
import Page.Library.CqlPanel as CqlPanel
import Page.Library.Sidebar as Sidebar
import Route exposing (Route)
import Session exposing (Server, Session)



-- MODEL


type alias Model =
    { session : Session
    , libraryId : Id
    , data : Status Data
    }


type alias Data =
    { library : Library
    , header : Header.Model
    , sidebar : Sidebar.Model
    , cqlPanel : CqlPanel.Model
    }


init : Session -> Id -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , libraryId = id
      , data = Loading
      }
    , loadLibrary (Session.getBase session) id
    )


toSession : Model -> Session
toSession model =
    model.session


updateSession : (Session -> Session) -> Model -> Model
updateSession f model =
    { model | session = f model.session }



-- UPDATE


type Msg
    = ClickedHeaderSave (Maybe String) (Maybe String)
    | ClickedHeaderDelete
    | ClickedCqlPanelSave (Maybe Attachment)
    | ClickedSidebarSave Library
    | ClickedSidebarCopyToServer Server
    | CompletedLoadLibrary (Result FhirHttp.Error Library)
    | CompletedSaveLibrary (Result FhirHttp.Error Library)
    | CompletedDuplicateLibrary (Result FhirHttp.Error Library)
    | CompletedDeleteLibrary (Result Http.Error ())
    | GotHeaderMsg Header.Msg
    | GotSidebarMsg Sidebar.Msg
    | GotCqlPanelMsg CqlPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Route )
update msg model =
    case msg of
        ClickedHeaderSave title description ->
            ( model
            , doWithData
                (\{ library } ->
                    { library
                        | title = title
                        , description = description
                    }
                        |> saveLibrary (Session.getBase model.session) model.libraryId
                )
                model
            , Nothing
            )

        ClickedHeaderDelete ->
            ( model
            , deleteLibrary (Session.getBase model.session) model.libraryId
            , Nothing
            )

        ClickedCqlPanelSave attachment ->
            ( model
            , doWithData
                (\{ library } ->
                    { library
                        | content = MaybeExtra.toList attachment
                    }
                        |> saveLibrary (Session.getBase model.session) model.libraryId
                )
                model
            , Nothing
            )

        ClickedSidebarSave library ->
            ( model
            , saveLibrary (Session.getBase model.session) model.libraryId library
            , Nothing
            )

        ClickedSidebarCopyToServer server ->
            ( model
            , case model.data of
                Loaded { library } ->
                    duplicateLibrary server.url library

                _ ->
                    Cmd.none
            , Nothing
            )

        CompletedLoadLibrary (Ok library) ->
            ( { model | data = loaded (Session.getBase model.session) library }
            , Cmd.none
            , Nothing
            )

        CompletedLoadLibrary (Err error) ->
            ( { model | data = Failed error }
            , Cmd.none
            , Nothing
            )

        CompletedSaveLibrary (Ok library) ->
            ( { model | data = loaded (Session.getBase model.session) library }
            , Cmd.none
            , Nothing
            )

        CompletedSaveLibrary (Err _) ->
            ( model, Cmd.none, Nothing )

        CompletedDuplicateLibrary (Ok library) ->
            ( { model | data = loaded (Session.getBase model.session) library }
            , Cmd.none
            , Nothing
            )

        CompletedDuplicateLibrary (Err _) ->
            ( model, Cmd.none, Nothing )

        CompletedDeleteLibrary (Ok _) ->
            ( model
            , Cmd.none
            , Just Route.LibraryList
            )

        CompletedDeleteLibrary (Err _) ->
            ( model, Cmd.none, Nothing )

        GotHeaderMsg msg_ ->
            let
                updateHeader f =
                    updateData (\data -> { data | header = f data.header })
            in
            ( updateHeader (Header.update msg_) model, Cmd.none, Nothing )

        GotSidebarMsg msg_ ->
            updateDataWithCmd
                (\data ->
                    let
                        ( newSidebar, cmd ) =
                            Sidebar.update msg_ data.sidebar
                    in
                    ( { data | sidebar = newSidebar }
                    , Cmd.map GotSidebarMsg cmd
                    )
                )
                model

        GotCqlPanelMsg msg_ ->
            let
                updateCqlPanel f =
                    updateData (\data -> { data | cqlPanel = f data.cqlPanel })
            in
            ( updateCqlPanel (CqlPanel.update msg_) model, Cmd.none, Nothing )


doWithData f model =
    case model.data of
        Loaded data ->
            f data

        _ ->
            Cmd.none


updateData f model =
    case model.data of
        Loaded data ->
            { model | data = Loaded (f data) }

        _ ->
            model


updateDataWithCmd f model =
    case model.data of
        Loaded data ->
            let
                ( data_, cmd ) =
                    f data
            in
            ( { model | data = Loaded data_ }, cmd, Nothing )

        _ ->
            ( model, Cmd.none, Nothing )


loaded base library =
    Loaded
        { library = library
        , header = Header.init library.title library.description
        , sidebar = Sidebar.init base library
        , cqlPanel = CqlPanel.init (library.content |> List.head)
        }


loadLibrary base id =
    FhirHttp.read CompletedLoadLibrary base "Library" id Library.decoder


saveLibrary base libraryId library =
    FhirHttp.update CompletedSaveLibrary
        base
        "Library"
        libraryId
        Library.decoder
        (Library.encode library)


duplicateLibrary base library =
    FhirHttp.create CompletedDuplicateLibrary
        base
        "Library"
        Library.decoder
        (Library.encode library)


deleteLibrary base libraryId =
    FhirHttp.delete CompletedDeleteLibrary base "Library" libraryId



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    { title = [ "Library" ]
    , content =
        div [ class "mt-16 ml-48 mr-64 p-6 flex-grow bg-gray-100" ] <|
            viewData model.session model.data
    }


viewData : Session -> Status Data -> List (Html Msg)
viewData session data =
    case data of
        Loading ->
            []

        LoadingSlowly ->
            []

        Loaded loadedData ->
            [ viewLibrary loadedData
            , viewSidebar session loadedData.sidebar
            ]

        Reloading _ ->
            []

        ReloadingSlowly _ ->
            []

        Failed error ->
            [ viewError error ]


viewLibrary : Data -> Html Msg
viewLibrary { library, header, cqlPanel } =
    Html.div []
        [ viewHeader header
        , viewCqlPanel cqlPanel
        ]


viewHeader header =
    Header.view
        { onSave = ClickedHeaderSave
        , onDelete = ClickedHeaderDelete
        , onMsg = GotHeaderMsg
        }
        header


viewCqlPanel cqlPanel =
    CqlPanel.view
        { onSave = ClickedCqlPanelSave
        , onMsg = GotCqlPanelMsg
        }
        cqlPanel


viewSidebar : Session -> Sidebar.Model -> Html Msg
viewSidebar session sidebar =
    Sidebar.view
        { servers = Zipper.toList session.servers
        , onMsg = GotSidebarMsg
        , onSave = ClickedSidebarSave
        , onCopyToServer = ClickedSidebarCopyToServer
        }
        sidebar


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
            Error.view error
