module Page exposing (..)

import Browser exposing (Document)
import Component.FontAwesome as Fa
import Html exposing (Html, div, h3, h6, span, text)
import Html.Attributes exposing (class)
import Material.Drawer.Permanent as Drawer
import Material.Icon as Icon
import Material.List as List
import Material.List.Divider as ListDivider
import Material.List.Item as ListItem
import Material.TopAppBar as TopAppBar
import Material.Typography as Typography
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
            [ Typography.typography
            , class "page"
            ]
            [ appBar session title
            , drawer config
            , Html.map toPageMsg content
            ]
        ]
    }


drawer config =
    Drawer.drawer
        (Drawer.config |> Drawer.setAttributes [ class "drawer" ])
        [ Drawer.header []
            [ h3 [ Drawer.title ] [ text "Blaze QR" ]
            , h6 [ Drawer.subtitle ]
                [ text "Quality Reporting" ]
            ]
        , Drawer.content [ class "drawer__content" ]
            [ List.list List.config
                [ ListItem.listItem
                    (ListItem.config
                        |> ListItem.setOnClick (config.onNavItemClick Libraries)
                    )
                    [ ListItem.graphic [] [ Fa.icon "book" ]
                    , text "Libraries"
                    ]
                , ListItem.listItem
                    (ListItem.config
                        |> ListItem.setOnClick (config.onNavItemClick Measures)
                    )
                    [ ListItem.graphic [] [ Fa.icon "calculator" ]
                    , text "Measures"
                    ]
                , ListDivider.listItem ListDivider.config
                , ListItem.listItem
                    (ListItem.config
                        |> ListItem.setOnClick (config.onNavItemClick Settings)
                    )
                    [ ListItem.graphic [] [ Icon.icon [] "settings" ]
                    , text "Settings"
                    ]
                , ListItem.listItem
                    (ListItem.config
                        |> ListItem.setOnClick (config.onNavItemClick Help)
                    )
                    [ ListItem.graphic [] [ Icon.icon [] "help" ]
                    , text "Help"
                    ]
                ]
            ]
        ]


appBar : Session -> List String -> Html msg
appBar session title =
    TopAppBar.regular
        (TopAppBar.config
            |> TopAppBar.setFixed True
            |> TopAppBar.setAttributes [ class "app-bar" ]
        )
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ span [ TopAppBar.title ]
                    [ text (String.join " " title) ]
                ]
            , TopAppBar.section [ TopAppBar.alignEnd ]
                [ span [ class "app-bar__server_name" ]
                    [ text (Session.activeServer session).name ]
                ]
            ]
        ]
