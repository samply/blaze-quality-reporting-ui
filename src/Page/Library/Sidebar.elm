module Page.Library.Sidebar exposing (Model, Msg, init, update, view)

import Component.Sidebar exposing (sidebar, sidebarConfig)
import Fhir.Library exposing (Library)
import Html exposing (Html)
import Page.Library.Sidebar.Url as Url



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
    = UrlMsg Url.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        UrlMsg msg_ ->
            { model | url = Url.update msg_ model.url }



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
        { onMsg = UrlMsg >> onMsg
        , onSave = \url -> onSave { library | url = url }
        }
        model.url
