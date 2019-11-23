module Page.Measure.List exposing (Model, Msg, init, toSession, update, view)

import Component.Error as Error
import Fhir.Bundle exposing (Bundle)
import Fhir.CodeableConcept as CodeableConcept
import Fhir.Expression as Expression
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeValue)
import Loading exposing (Status(..))
import Material.Button exposing (buttonConfig, textButton)
import Material.Fab exposing (fab, fabConfig)
import Material.List exposing (list, listConfig, listItem, listItemConfig)
import Route
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { session : Session
    , measures : Status (List Measure)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , measures = Loading
      }
    , searchMeasures (Session.getBase session) ""
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ClickedMeasure Measure
    | ClickedCreateMeasure
    | CompletedLoadMeasures (Result FhirHttp.Error Bundle)
    | CompletedCreateMeasure (Result FhirHttp.Error Measure)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMeasure measure ->
            ( model
            , measure.id
                |> Maybe.map (pushMeasureUrl model)
                |> Maybe.withDefault Cmd.none
            )

        ClickedCreateMeasure ->
            ( model, createMeasure (Session.getBase model.session) )

        CompletedLoadMeasures (Ok bundle) ->
            ( { model | measures = Loaded (decodeMeasures bundle) }
            , Cmd.none
            )

        CompletedLoadMeasures (Err error) ->
            ( { model | measures = Failed error }
            , Cmd.none
            )

        CompletedCreateMeasure (Ok measure) ->
            ( model
            , measure.id
                |> Maybe.map (pushMeasureUrl model)
                |> Maybe.withDefault Cmd.none
            )

        CompletedCreateMeasure (Err _) ->
            ( model, Cmd.none )


pushMeasureUrl model id =
    Route.pushUrl (Session.toNavKey model.session) (Route.Measure id)


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


createMeasure : String -> Cmd Msg
createMeasure base =
    let
        measure =
            { id = Nothing
            , name = Nothing
            , title = Nothing
            , subtitle = Nothing
            , subject =
                Just
                    { coding =
                        [ { system = Just "http://hl7.org/fhir/resource-types"
                          , version = Nothing
                          , code = Just "Patient"
                          }
                        ]
                    , text = Nothing
                    }
            , description = Nothing
            , library = []
            , scoring =
                Just (CodeableConcept.ofOneCoding (Measure.scoring "cohort"))
            , group =
                [ { code = Nothing
                  , description = Nothing
                  , population =
                        [ { code =
                                Just
                                    (CodeableConcept.ofOneCoding
                                        (Measure.populationType
                                            "initial-population"
                                        )
                                    )
                          , description = Nothing
                          , criteria =
                                Expression.cql (Just "InInitialPopulation")
                          }
                        ]
                  , stratifier = []
                  }
                ]
            }
    in
    FhirHttp.create
        CompletedCreateMeasure
        base
        "Measure"
        Measure.decoder
        (Measure.encode measure)



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    { title = [ "Measures" ]
    , content =
        case model.measures of
            Loaded measures ->
                if List.isEmpty measures then
                    emptyListPlaceholder

                else
                    measureList measures

            Loading ->
                text ""

            LoadingSlowly ->
                text ""

            Failed error ->
                div [ class "main-content measure-list=page" ]
                    [ Error.view error ]
    }


emptyListPlaceholder =
    div [ class "main-content measure-list__empty-placeholder" ]
        [ textButton
            { buttonConfig | onClick = Just ClickedCreateMeasure }
            "create the first measure"
        ]


measureList measures =
    div [ class "main-content measure-list" ]
        [ createButton
        , list listConfig <|
            List.map measureListItem measures
        ]


measureListItem measure =
    listItem { listItemConfig | onClick = Just <| ClickedMeasure measure }
        [ text <| measureTitle measure ]


measureTitle { title, name, id } =
    List.filterMap identity [ title, name, id ]
        |> List.head
        |> Maybe.withDefault "<unknown>"


createButton =
    fab
        { fabConfig
            | onClick = Just ClickedCreateMeasure
            , additionalAttributes = [ class "measure-list__create-button" ]
        }
        "add"
