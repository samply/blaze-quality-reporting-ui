module Page.Help exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , view
    )

import Html exposing (Html, a, div, h2, li, text, ul)
import Html.Attributes exposing (class, href)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> { title : List String, content : Html Msg }
view _ =
    { title = [ "Help" ]
    , content =
        div [ class "main-content help-page" ]
            [ h2 [ class "mdc-typography--headline5" ] [ text "References" ]
            , ul []
                [ li []
                    [ a [ href "http://hl7.org/fhir/us/cqfmeasures/STU1" ]
                        [ text "Quality Measure STU1 for FHIR STU3 Implementation Guide" ]
                    ]
                , li []
                    [ a [ href "http://build.fhir.org/ig/HL7/cqf-measures/branches/R4_Lift/index.html" ]
                        [ text "Quality Measure STU2 for FHIR R4 Implementation Guide (work in progress)" ]
                    ]
                , li []
                    [ a [ href "https://build.fhir.org/ig/HL7/davinci-deqm/" ]
                        [ text "Data Exchange For Quality Measures STU1 for FHIR STU3 Implementation Guide" ]
                    ]
                , li []
                    [ a [ href "https://ecqi.healthit.gov/ecqm-and-ecqi-education-resources" ]
                        [ text "eCQM and eCQI Educational Resources" ]
                    ]
                , li []
                    [ a [ href "https://ecqi.healthit.gov/sites/default/files/eCQM-LogicAndGuidance-2019-05-v3-1.pdf" ]
                        [ text "Electronic Clinical Quality Measure Logic and Implementation Guidance" ]
                    ]
                , li []
                    [ a [ href "https://www.hl7.org/fhir/clinicalreasoning-quality-reporting.html" ]
                        [ text "Quality Reporting" ]
                    ]
                , li []
                    [ a [ href "https://www.hl7.org/fhir/library.html" ]
                        [ text "Library Resource" ]
                    ]
                , li []
                    [ a [ href "https://www.hl7.org/fhir/measure.html" ]
                        [ text "Measure Resource" ]
                    ]
                , li []
                    [ a [ href "https://www.hl7.org/fhir/measurereport.html" ]
                        [ text "MeasureReport Resource" ]
                    ]
                ]
            ]
    }
