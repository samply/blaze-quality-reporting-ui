module Component.FontAwesome exposing (..)

import Html exposing (Html, i)
import Html.Attributes exposing (class)


icon : String -> Html msg
icon name =
    i [ class ("fas fa-" ++ name) ] []
