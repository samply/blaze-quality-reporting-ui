module Page.Library exposing (Model, Msg, init, toSession, update, view)

import Component.Header as Header
import Fhir.Attachment exposing (Attachment)
import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Http as FhirHttp
import Fhir.Library as Library exposing (Library)
import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import List.Extra exposing (getAt, removeAt, setAt, updateAt)
import Loading exposing (Status(..))
import Material.Button exposing (buttonConfig, outlinedButton)
import Material.Card
    exposing
        ( card
        , cardActionButton
        , cardActions
        , cardBlock
        , cardConfig
        )
import Material.LayoutGrid
    exposing
        ( layoutGrid
        , layoutGridCell
        , layoutGridInner
        , span12
        )
import Maybe.Extra as MaybeExtra
import Page.Library.CqlPanel as CqlPanel
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , libraryId : Id
    , data : Status Data
    }


type alias Data =
    { library : Library
    , header : Header.Model
    , cqlPanel : CqlPanel.Model
    }


init : Session -> Id -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , libraryId = id
      , data = Loading
      }
    , loadLibrary session.base id
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ClickedHeaderSave (Maybe String) (Maybe String)
    | ClickedHeaderDelete
    | ClickedCqlPanelSave (Maybe Attachment)
    | CompletedLoadLibrary (Result Http.Error Library)
    | CompletedSaveLibrary (Result Http.Error Library)
    | CompletedDeleteLibrary (Result Http.Error ())
    | HeaderMsg Header.Msg
    | CqlPanelMsg CqlPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
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
                        |> saveLibrary model.session.base model.libraryId
                )
                model
            )

        ClickedHeaderDelete ->
            ( model
            , deleteLibrary model.session.base model.libraryId
            )

        ClickedCqlPanelSave attachment ->
            ( model
            , doWithData
                (\{ library } ->
                    { library
                        | content = MaybeExtra.toList attachment
                    }
                        |> saveLibrary model.session.base model.libraryId
                )
                model
            )

        CompletedLoadLibrary (Ok library) ->
            ( { model | data = loaded library }, Cmd.none )

        CompletedLoadLibrary (Err _) ->
            ( { model | data = Failed }
            , Cmd.none
            )

        CompletedSaveLibrary (Ok library) ->
            ( { model | data = loaded library }, Cmd.none )

        CompletedSaveLibrary (Err _) ->
            ( model, Cmd.none )

        CompletedDeleteLibrary (Ok _) ->
            ( model
            , Route.pushUrl (Session.navKey model.session) Route.LibraryList
            )

        CompletedDeleteLibrary (Err _) ->
            ( model, Cmd.none )

        HeaderMsg msg_ ->
            let
                updateHeader f =
                    updateData (\data -> { data | header = f data.header })
            in
            ( updateHeader (Header.update msg_) model, Cmd.none )

        CqlPanelMsg msg_ ->
            let
                updateCqlPanel f =
                    updateData (\data -> { data | cqlPanel = f data.cqlPanel })
            in
            ( updateCqlPanel (CqlPanel.update msg_) model, Cmd.none )


doWithData : (Data -> Cmd Msg) -> Model -> Cmd Msg
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


loaded library =
    Loaded
        { library = library
        , header = Header.init library.title library.description
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


deleteLibrary base libraryId =
    FhirHttp.delete CompletedDeleteLibrary base "Library" libraryId



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    case model.data of
        Loaded data ->
            { title = [ "Library" ]
            , content =
                div [ class "main-content library-page" ]
                    [ viewLibrary data

                    --, viewRightSidebar data.library
                    ]
            }

        Loading ->
            { title = [ "Library" ]
            , content =
                div [ class "main-content" ] []
            }

        LoadingSlowly ->
            { title = [ "Library" ]
            , content =
                div [ class "main-content" ] []
            }

        Failed ->
            { title = [ "Library" ]
            , content =
                div [ class "main-content" ] []
            }



{- viewRightSidebar : Library -> Html Msg
   viewRightSidebar library =
       let
           config =
               { onEdit = ClickedRightSidebarLibraryEdit }
       in
       div [ class "library-page__right-sidebar right-sidebar" ]
           [ SidebarLibrary.view config library.library ]
-}


viewLibrary : Data -> Html Msg
viewLibrary { library, header, cqlPanel } =
    layoutGrid [ class "library" ]
        [ layoutGridInner []
            [ viewHeader header
            , viewCqlPanel cqlPanel
            ]
        ]


viewHeader header =
    Header.view
        { onSave = ClickedHeaderSave
        , onDelete = ClickedHeaderDelete
        , onMsg = HeaderMsg
        }
        header


viewCqlPanel cqlPanel =
    CqlPanel.view
        { onSave = ClickedCqlPanelSave
        , onMsg = CqlPanelMsg
        }
        cqlPanel
