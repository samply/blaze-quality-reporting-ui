module Page.Library.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar exposing (sidebar, sidebarConfig)
import Component.Sidebar.Url as Url
import Fhir.Library exposing (Library)
import Html exposing (Html)



-- MODEL


type alias Model =
    { library : Library
    , url : Url.Model
    }


init : Library -> Model
init library =
    { library = library
    , url = Url.init library.url
    }



-- UPDATE


type Msg
    = GotUrlMsg Url.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUrlMsg msg_ ->
            let
                ( url, cmd ) =
                    Url.update msg_ model.url
            in
            ( { model | url = url }, Cmd.map GotUrlMsg cmd )



-- VIEW


type alias Config msg =
    { onMsg : Msg -> msg
    , onSave : Library -> msg
    }


view : Config msg -> Model -> Html msg
view config model =
    sidebar sidebarConfig
        [ viewUrl config model ]


viewUrl { onMsg, onSave } ({ library } as model) =
    Url.view
        { onMsg = GotUrlMsg >> onMsg
        , onSave = \url -> onSave { library | url = url }
        }
        model.url
