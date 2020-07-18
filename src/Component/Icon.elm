module Component.Icon exposing (icon)

import Html exposing (Html, text)
import Html.Attributes exposing (class)


{-| Icon view function
-}
icon : List (Html.Attribute msg) -> String -> Html msg
icon additionalAttributes iconName =
    Html.i (class "material-icons" :: additionalAttributes) [ text iconName ]
