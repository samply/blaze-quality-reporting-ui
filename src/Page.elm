module Page exposing (..)

import Browser exposing (Document)
import Component.Icon as Icon
import Component.List as List
import Component.List.Item as ListItem
import Html exposing (Html, div, h3, h6, text)
import Html.Attributes exposing (class)
import Session exposing (Session)


type NavItem
    = Libraries
    | Measures
    | Settings
    | Help


type alias Config msg =
    { onDrawerClose : msg
    , onNavIconClick : msg
    , onNavItemClick : NavItem -> msg
    }


{-| Take a page's Html and frames it with a header and footer.
-}
view :
    (pageMsg -> msg)
    -> Config msg
    -> Session
    -> { title : List String, content : Html pageMsg }
    -> Document msg
view toPageMsg config session { title, content } =
    { title =
        "Blaze Quality Reporting"
            :: title
            |> List.reverse
            |> String.join " - "
    , body =
        [ div
            [ class "" ]
            [ appBar session title
            , drawer config
            , Html.map toPageMsg content
            ]
        ]
    }


appBar : Session -> List String -> Html msg
appBar session title =
    Html.div [ class "fixed top-0 inset-x-0 ml-48 px-4 py-5 h-16 bg-gray-800" ]
        [ Html.div [ class "flex justify-between" ]
            [ Html.div [ class "text-white" ]
                [ text (String.join " " title) ]
            , Html.div [ class "text-white" ]
                [ text (Session.activeServer session).name ]
            ]
        ]


drawer config =
    Html.div [ class "fixed inset-y-0 w-48 p-4 bg-gray-800" ]
        [ Html.div [ class "mb-4" ]
            [ h3 [ class "text-white text-lg" ]
                [ text "Blaze QR" ]
            , h6 [ class "text-gray-200 text-sm" ]
                [ text "Quality Reporting" ]
            ]
        , List.list List.config
            [ ListItem.listItem
                (ListItem.config
                    |> ListItem.setOnClick (config.onNavItemClick Libraries)
                )
                "libraries"
                [ ListItem.graphic [] [ Icon.icon [] "book" ]
                , text "Libraries"
                ]
            , ListItem.listItem
                (ListItem.config
                    |> ListItem.setOnClick (config.onNavItemClick Measures)
                )
                "measures"
                [ ListItem.graphic [] [ Icon.icon [] "calculate" ]
                , text "Measures"
                ]
            , ListItem.listItem
                (ListItem.config
                    |> ListItem.setOnClick (config.onNavItemClick Settings)
                )
                "settings"
                [ ListItem.graphic [] [ Icon.icon [] "settings" ]
                , text "Settings"
                ]
            , ListItem.listItem
                (ListItem.config
                    |> ListItem.setOnClick (config.onNavItemClick Help)
                )
                "help"
                [ ListItem.graphic [] [ Icon.icon [] "help" ]
                , text "Help"
                ]
            ]
        ]
