module Page.Measure.List exposing (Model, Msg, init, toSession, update, view)

import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Html, text)
import Http
import Json.Decode exposing (decodeValue)
import Material.List exposing (list, listConfig, listItem, listItemConfig)
import Route
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { session : Session
    , measures : Status (List Measure)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , measures = Loading
      }
    , searchMeasures session.base ""
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ClickedMeasure Measure
    | CompletedLoadMeasures (Result Http.Error Bundle)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMeasure measure ->
            ( model
            , measure.id
                |> Maybe.map (pushMeasureUrl model)
                |> Maybe.withDefault Cmd.none
            )

        CompletedLoadMeasures (Ok bundle) ->
            ( { model | measures = Loaded (decodeMeasures bundle) }
            , Cmd.none
            )

        CompletedLoadMeasures (Err _) ->
            ( { model | measures = Failed }
            , Cmd.none
            )


pushMeasureUrl model id =
    Route.pushUrl (Session.navKey model.session) (Route.Measure id)


searchMeasures base query =
    let
        params =
            if String.isEmpty query then
                []

            else
                [ UrlBuilder.string "title:contains" query ]
    in
    FhirHttp.searchType CompletedLoadMeasures base "Measure" params


decodeMeasures : Bundle -> List Measure
decodeMeasures { entry } =
    List.filterMap
        (.resource >> decodeValue Measure.decoder >> Result.toMaybe)
        entry



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    { title = [ "Measures" ]
    , content =
        case model.measures of
            Loaded measures ->
                measureList measures

            Loading ->
                text ""

            LoadingSlowly ->
                text ""

            Failed ->
                text "error"
    }


measureList measures =
    list listConfig <|
        List.map measureListItem measures


measureListItem measure =
    listItem { listItemConfig | onClick = Just <| ClickedMeasure measure }
        [ text <| measureTitle measure ]


measureTitle { title, name, id } =
    List.filterMap identity [ title, name, id ]
        |> List.head
        |> Maybe.withDefault "<unknown>"
