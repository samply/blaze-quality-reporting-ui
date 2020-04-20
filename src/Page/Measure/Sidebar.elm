module Page.Measure.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar as Sidebar
import Component.Sidebar.SharePanel as SharePanel
import Component.Sidebar.UrlPanel as UrlPanel
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Html)
import Page.Measure.Sidebar.Library as Library
import Page.Measure.Sidebar.Subject as Subject
import Session exposing (Server)



-- MODEL


type alias Model =
    { measure : Measure
    , sharePanel : SharePanel.Model
    , urlPanel : UrlPanel.Model
    , subject : Subject.Model
    , library : Library.Model
    }


init : String -> Measure -> ( Model, Cmd Msg )
init base measure =
    let
        ( library, cmd ) =
            Library.init base (List.head measure.library)
    in
    ( { measure = measure
      , sharePanel = SharePanel.init
      , urlPanel = UrlPanel.init measure.url
      , subject = Subject.init (Measure.getSubjectCode measure)
      , library = library
      }
    , Cmd.map GotLibraryMsg cmd
    )



-- UPDATE


type Msg
    = GotShareMsg SharePanel.Msg
    | GotUrlMsg UrlPanel.Msg
    | GotSubjectMsg Subject.Msg
    | GotLibraryMsg Library.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotShareMsg msg_ ->
            let
                ( share, cmd ) =
                    SharePanel.update msg_ model.sharePanel
            in
            ( { model | sharePanel = share }, Cmd.map GotShareMsg cmd )

        GotUrlMsg msg_ ->
            let
                ( url, cmd ) =
                    UrlPanel.update msg_ model.urlPanel
            in
            ( { model | urlPanel = url }, Cmd.map GotUrlMsg cmd )

        GotSubjectMsg msg_ ->
            let
                ( subject, cmd ) =
                    Subject.update msg_ model.subject
            in
            ( { model | subject = subject }, Cmd.map GotSubjectMsg cmd )

        GotLibraryMsg msg_ ->
            ( { model | library = Library.update msg_ model.library }
            , Cmd.none
            )



-- VIEW


type alias Config msg =
    { servers : List Server
    , onMsg : Msg -> msg
    , onSave : Measure -> msg
    , onLibraryEdit : msg
    , onCopyToServer : Server -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    Sidebar.view Sidebar.config
        [ viewSharePanel config model
        , viewUrlPanel config model
        , viewSubject config model
        , viewLibrary config model
        ]


viewSharePanel { servers, onMsg, onCopyToServer } { sharePanel } =
    SharePanel.view
        { servers = servers
        , onMsg = GotShareMsg >> onMsg
        , onCopyToServer = onCopyToServer
        }
        sharePanel


viewUrlPanel { onMsg, onSave } { measure, urlPanel } =
    UrlPanel.view
        { onMsg = GotUrlMsg >> onMsg
        , onSave = \url -> onSave { measure | url = url }
        }
        urlPanel


viewSubject { onMsg, onSave } ({ measure } as model) =
    Subject.view
        { onMsg = GotSubjectMsg >> onMsg
        , onSave = \code -> onSave (Measure.setSubjectCode code measure)
        }
        model.subject


viewLibrary { onLibraryEdit } { library } =
    Library.view { onEdit = onLibraryEdit } library
