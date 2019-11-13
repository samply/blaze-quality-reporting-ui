module Page.Measure exposing (Model, Msg, init, toSession, update, view)

import Fhir.CodeableConcept exposing (CodeableConcept)
import Fhir.Http as FhirHttp
import Fhir.Measure as Measure exposing (Measure)
import Fhir.PrimitiveTypes exposing (Canonical, Id)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import List.Extra exposing (getAt, updateAt)
import Material.Button exposing (buttonConfig)
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
        , span8
        )
import Material.List
    exposing
        ( list
        , listConfig
        , listGroup
        , listGroupSubheader
        , listItem
        , listItemConfig
        , listItemPrimaryText
        , listItemSecondaryText
        , listItemText
        )
import Page.Measure.PopulationDialog as PopulationDialog
import Page.Measure.StratifierDialog as StratifierDialog
import Route exposing (href)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , populationDialog : PopulationDialog.Model Msg
    , stratifierDialog : StratifierDialog.Model Msg
    , measureId : Id
    , measure : Status Measure
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


init : Session -> Id -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , populationDialog =
            PopulationDialog.init session.base PopulationDialogMsg SavedMeasure
      , stratifierDialog =
            StratifierDialog.init session.base StratifierDialogMsg SavedMeasure
      , measureId = id
      , measure = Loading
      }
    , loadMeasure session.base id
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ClickedPopulation Int Int
    | ClickedAddStratifier Int
    | SavedMeasure Measure
    | CompletedLoadMeasure (Result Http.Error Measure)
    | PopulationDialogMsg PopulationDialog.Msg
    | StratifierDialogMsg StratifierDialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPopulation groupIdx populationIdx ->
            ( case model.measure of
                Loaded measure ->
                    updatePopulationDialog
                        (PopulationDialog.doOpen model.measureId
                            measure
                            groupIdx
                            populationIdx
                        )
                        model

                _ ->
                    model
            , Cmd.none
            )

        ClickedAddStratifier groupIdx ->
            ( case model.measure of
                Loaded measure ->
                    updateStratifierDialog
                        (StratifierDialog.doOpen model.measureId
                            (addNewStratifier groupIdx measure)
                            groupIdx
                            (Debug.log "idx"
                                (getAt groupIdx measure.group
                                    |> Maybe.map .stratifier
                                    |> Maybe.map List.length
                                    |> Maybe.withDefault 0
                                )
                            )
                        )
                        model

                _ ->
                    model
            , Cmd.none
            )

        SavedMeasure measure ->
            ( { model | measure = Loaded measure }, Cmd.none )

        CompletedLoadMeasure (Ok measure) ->
            ( { model | measure = Loaded measure }
            , Cmd.none
            )

        CompletedLoadMeasure (Err _) ->
            ( { model | measure = Failed }
            , Cmd.none
            )

        PopulationDialogMsg msg_ ->
            let
                ( populationDialog, cmd ) =
                    PopulationDialog.update msg_ model.populationDialog
            in
            ( { model | populationDialog = populationDialog }, cmd )

        StratifierDialogMsg msg_ ->
            let
                ( stratifierDialog, cmd ) =
                    StratifierDialog.update msg_ model.stratifierDialog
            in
            ( { model | stratifierDialog = stratifierDialog }, cmd )


addNewStratifier : Int -> Measure -> Measure
addNewStratifier groupIdx =
    updateGroup
        (addStratifier
            { code = Nothing, description = Nothing, criteria = Nothing }
        )
        groupIdx


addStratifier : Measure.Stratifier -> Measure.Group -> Measure.Group
addStratifier stratifier group =
    { group | stratifier = group.stratifier ++ [ stratifier ] }


updateGroup : (Measure.Group -> Measure.Group) -> Int -> Measure -> Measure
updateGroup f groupIdx measure =
    { measure | group = updateAt groupIdx f measure.group }


updatePopulationDialog f model =
    { model | populationDialog = f model.populationDialog }


updateStratifierDialog f model =
    { model | stratifierDialog = f model.stratifierDialog }


loadMeasure base id =
    FhirHttp.read CompletedLoadMeasure base "Measure" id Measure.decoder



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view model =
    case model.measure of
        Loaded measure ->
            { title = [ "Measure", measureTitle measure ]
            , content =
                div []
                    [ viewPopulationDialog model.populationDialog
                    , viewStratifierDialog model.stratifierDialog
                    , viewMeasure measure
                    ]
            }

        Loading ->
            { title = [ "Measure" ]
            , content =
                div []
                    [ viewPopulationDialog model.populationDialog
                    , viewStratifierDialog model.stratifierDialog
                    ]
            }

        LoadingSlowly ->
            { title = [ "Measure" ]
            , content =
                div []
                    [ viewPopulationDialog model.populationDialog
                    , viewStratifierDialog model.stratifierDialog
                    ]
            }

        Failed ->
            { title = [ "Measure" ]
            , content =
                div []
                    [ viewPopulationDialog model.populationDialog
                    , viewStratifierDialog model.stratifierDialog
                    ]
            }


measureTitle { title, name, id } =
    List.filterMap identity [ title, name, id ]
        |> List.head
        |> Maybe.withDefault "<unknown>"


viewPopulationDialog : PopulationDialog.Model Msg -> Html Msg
viewPopulationDialog model =
    PopulationDialog.view model |> Html.map PopulationDialogMsg


viewStratifierDialog : StratifierDialog.Model Msg -> Html Msg
viewStratifierDialog model =
    StratifierDialog.view model |> Html.map StratifierDialogMsg


viewMeasure : Measure -> Html Msg
viewMeasure measure =
    layoutGrid []
        [ layoutGridInner []
            ([ layoutGridCell [] [ text "Title" ]
             , layoutGridCell [ span8 ] [ text (defaultNotSpecified measure.title) ]
             , layoutGridCell [] [ text "Subtitle" ]
             , layoutGridCell [ span8 ] [ text (defaultNotSpecified measure.subtitle) ]
             , layoutGridCell [] [ text "Subject" ]
             , layoutGridCell [ span8 ]
                [ text (defaultNotSpecified (firstCode measure.subject)) ]
             , layoutGridCell [] [ text "Library" ]
             , layoutGridCell [ span8 ] [ libraryLink measure.library ]
             , layoutGridCell [] [ text "Scoring" ]
             , layoutGridCell [ span8 ]
                [ text (defaultNotSpecified (firstCode measure.scoring)) ]
             , layoutGridCell [ span12 ]
                [ h2 [ class "mdc-typography--headline6" ] [ text "Groups" ] ]
             ]
                ++ List.indexedMap
                    (\idx group -> layoutGridCell [ span12 ] [ viewGroup idx group ])
                    measure.group
            )
        ]


defaultNotSpecified =
    Maybe.withDefault "<not specified>"


firstCode : Maybe CodeableConcept -> Maybe String
firstCode concept =
    concept
        |> Maybe.map .coding
        |> Maybe.andThen List.head
        |> Maybe.andThen .code


libraryLink : List Canonical -> Html Msg
libraryLink uris =
    uris
        |> List.head
        |> Maybe.map (\uri -> a [ href (Route.LibraryByUrl uri) ] [ text uri ])
        |> Maybe.withDefault (text "no associated library")


viewGroup : Int -> Measure.Group -> Html Msg
viewGroup idx { code, description, population, stratifier } =
    card { cardConfig | additionalAttributes = [ class "measure-group-card" ] }
        { blocks =
            [ cardBlock <|
                div [ class "measure-group-card__header" ]
                    [ h3
                        [ class "measure-group-card__title"
                        , class "mdc-typography--headline6"
                        ]
                        [ firstCode code
                            |> Maybe.withDefault ("Group " ++ String.fromInt (idx + 1))
                            |> text
                        ]
                    ]
            , cardBlock <|
                listGroup [] <|
                    [ listGroupSubheader [] [ text "Populations" ]
                    , list { listConfig | twoLine = True }
                        (List.indexedMap (viewPopulation idx) population)
                    ]
                        ++ (if List.isEmpty stratifier then
                                []

                            else
                                [ listGroupSubheader [] [ text "Stratifiers" ]
                                , list { listConfig | twoLine = True } <|
                                    List.map viewStratifier stratifier
                                ]
                           )
            ]
        , actions =
            Just <|
                cardActions
                    { buttons =
                        [ cardActionButton buttonConfig
                            "Add Population"
                        , cardActionButton
                            { buttonConfig
                                | onClick = Just (ClickedAddStratifier idx)
                            }
                            "Add Stratifier"
                        ]
                    , icons = []
                    }
        }


viewPopulation : Int -> Int -> Measure.Population -> Html Msg
viewPopulation groupIdx populationIdx { code, criteria } =
    listItem
        { listItemConfig
            | onClick = Just (ClickedPopulation groupIdx populationIdx)
        }
        [ listItemText []
            [ listItemPrimaryText []
                [ text (defaultNotSpecified (firstCode code)) ]
            , listItemSecondaryText []
                [ text (defaultNotSpecified criteria.expression) ]
            ]
        ]


viewStratifier : Measure.Stratifier -> Html Msg
viewStratifier { code, description } =
    listItem listItemConfig
        [ listItemText []
            [ listItemPrimaryText []
                [ text (defaultNotSpecified (firstCode code)) ]
            , listItemSecondaryText []
                [ text (defaultNotSpecified description) ]
            ]
        ]
