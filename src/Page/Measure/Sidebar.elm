module Page.Measure.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar exposing (sidebar, sidebarConfig)
import Component.Sidebar.Url as Url
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Html)
import Page.Measure.Sidebar.Library as Library
import Page.Measure.Sidebar.Subject as Subject



-- MODEL


type alias Model =
    { measure : Measure
    , url : Url.Model
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
      , url = Url.init measure.url
      , subject = Subject.init (Measure.getSubjectCode measure)
      , library = library
      }
    , Cmd.map GotLibraryMsg cmd
    )



-- UPDATE


type Msg
    = GotUrlMsg Url.Msg
    | GotSubjectMsg Subject.Msg
    | GotLibraryMsg Library.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUrlMsg msg_ ->
            let
                ( url, cmd ) =
                    Url.update msg_ model.url
            in
            ( { model | url = url }, Cmd.map GotUrlMsg cmd )

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
    { onMsg : Msg -> msg
    , onSave : Measure -> msg
    , onLibraryEdit : msg
    }


view : Config msg -> Model -> Html msg
view config model =
    sidebar sidebarConfig
        [ viewUrl config model
        , viewSubject config model
        , viewLibrary config model
        ]


viewUrl { onMsg, onSave } ({ measure } as model) =
    Url.view
        { onMsg = GotUrlMsg >> onMsg
        , onSave = \url -> onSave { measure | url = url }
        }
        model.url


viewSubject { onMsg, onSave } ({ measure } as model) =
    Subject.view
        { onMsg = GotSubjectMsg >> onMsg
        , onSave = \code -> onSave (Measure.setSubjectCode code measure)
        }
        model.subject


viewLibrary { onLibraryEdit } { library } =
    Library.view { onEdit = onLibraryEdit } library
