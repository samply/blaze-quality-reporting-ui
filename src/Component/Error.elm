module Component.Error exposing (view)

import Fhir.Http as FhirHttp
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


view : FhirHttp.Error -> Html msg
view error =
    case Debug.log "error" error of
        FhirHttp.NetworkError ->
            div [ class "error" ]
                [ div [ class "error__big-http-status" ]
                    [ text "Network Error" ]
                , div [ class "error__big-http-status-message" ]
                    [ text "Please check your network connection." ]
                ]

        FhirHttp.BadStatus status _ ->
            case status of
                404 ->
                    div [ class "error" ]
                        [ div [ class "error__big-http-status" ]
                            [ text "404" ]
                        , div [ class "error__big-http-status-message" ]
                            [ text "Not Found" ]
                        ]

                _ ->
                    div [ class "error" ]
                        [ text "Other Error" ]

        _ ->
            div [ class "error" ]
                [ text "Other Error" ]
