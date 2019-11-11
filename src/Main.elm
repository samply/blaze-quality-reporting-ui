module Main exposing (Msg(..), main, update, view)

import Browser
import Fhir.Bundle exposing (Bundle)
import Fhir.Http as FhirHttp
import Fhir.Library exposing (Library)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode as Encode
import Library.Panel as LibraryPanel
import Material.Button exposing (buttonConfig, raisedButton)
import Material.LayoutGrid
    exposing
        ( layoutGrid
        , layoutGridCell
        , layoutGridInner
        , span12
        )
import Material.TextField exposing (textField, textFieldConfig)
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Typography as Typography
import Measure.Panel as MeasurePanel


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { baseUrl : String
    , libraryPanel : LibraryPanel.Model Msg
    , measurePanel : MeasurePanel.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        base =
            --"https://blaze.life.uni-leipzig.de/fhir"
            --"http://hapi.fhir.org/baseR4"
            "http://localhost:8000/fhir"
    in
    ( { baseUrl = base
      , libraryPanel = LibraryPanel.init base LibraryPanelMsg LibraryChanged
      , measurePanel = MeasurePanel.init base Nothing
      }
    , Cmd.none
    )


type Msg
    = ChangeBaseUrl String
    | LibraryPanelMsg LibraryPanel.Msg
    | MeasurePanelMsg MeasurePanel.Msg
    | Execute
    | BundleResult (Result Http.Error Bundle)
    | LibraryChanged Library


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBaseUrl baseUrl ->
            ( { model
                | baseUrl = baseUrl
                , libraryPanel = LibraryPanel.init baseUrl LibraryPanelMsg LibraryChanged
              }
            , Cmd.none
            )

        LibraryPanelMsg msg_ ->
            let
                ( library, cmd ) =
                    LibraryPanel.update msg_ model.libraryPanel
            in
            ( { model | libraryPanel = library }
            , cmd
            )

        MeasurePanelMsg msg_ ->
            let
                ( measure, cmd ) =
                    MeasurePanel.update msg_ model.measurePanel
            in
            ( { model | measurePanel = measure }
            , Cmd.map MeasurePanelMsg cmd
            )

        Execute ->
            ( model
            , FhirHttp.postBundle BundleResult model.baseUrl Encode.null
            )

        BundleResult (Ok bundle) ->
            Debug.log (Debug.toString bundle) ( model, Cmd.none )

        BundleResult (Err error) ->
            Debug.log (Debug.toString error) ( model, Cmd.none )

        LibraryChanged library ->
            ( { model
                | measurePanel =
                    MeasurePanel.updateLibraryUrl library.url model.measurePanel
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ Typography.typography ]
        [ appBar
        , content model
        ]


appBar =
    topAppBar { topAppBarConfig | fixed = True }
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ Html.span [ TopAppBar.title ]
                    [ text "Blaze CDS" ]
                ]
            ]
        ]


baseUrlInput baseUrl =
    div [ style "display" "flex" ]
        [ textField
            { textFieldConfig
                | additionalAttributes = [ style "flex-grow" "1" ]
                , label = Just "Base URL"
                , value = Just baseUrl
                , onInput = Just ChangeBaseUrl
            }
        ]


content : Model -> Html Msg
content { baseUrl, libraryPanel, measurePanel } =
    div [ class "mdc-top-app-bar--fixed-adjust" ]
        [ layoutGrid []
            [ layoutGridInner []
                [ layoutGridCell [ span12 ] [ baseUrlInput baseUrl ]
                , layoutGridCell [ span12 ]
                    [ LibraryPanel.view libraryPanel |> Html.map LibraryPanelMsg
                    ]
                , layoutGridCell [ span12 ]
                    [ MeasurePanel.view measurePanel |> Html.map MeasurePanelMsg ]
                , layoutGridCell [ span12 ]
                    [ raisedButton
                        { buttonConfig
                            | onClick = Just Execute
                            , disabled = True
                        }
                        "Execute"
                    ]
                ]
            ]
        ]
