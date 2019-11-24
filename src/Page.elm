module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, h3, h6, span, text)
import Html.Attributes exposing (class)
import Material.Drawer
    exposing
        ( drawerContent
        , drawerHeader
        , drawerSubtitle
        , drawerTitle
        , permanentDrawer
        , permanentDrawerConfig
        )
import Material.Icon exposing (icon, iconConfig)
import Material.List
    exposing
        ( list
        , listConfig
        , listItem
        , listItemConfig
        , listItemDivider
        , listItemDividerConfig
        , listItemGraphic
        )
import Material.TopAppBar as TopAppBar exposing (topAppBar, topAppBarConfig)
import Material.Typography as Typography
import Session exposing (Session)


type NavItem
    = Libraries
    | Measures
    | Settings


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
    permanentDrawer
        { permanentDrawerConfig | additionalAttributes = [ class "drawer" ] }
        [ drawerHeader []
            [ h3 [ drawerTitle ] [ text "Blaze QR" ]
            , h6 [ drawerSubtitle ]
                [ text "Quality Reporting" ]
            ]
        , drawerContent [ class "drawer__content" ]
            [ list listConfig
                [ listItem
                    { listItemConfig
                        | onClick = Just (config.onNavItemClick Libraries)
                    }
                    [ listItemGraphic [] [ icon iconConfig "library_books" ]
                    , text "Libraries"
                    ]
                , listItem
                    { listItemConfig
                        | onClick = Just (config.onNavItemClick Measures)
                    }
                    [ listItemGraphic [] [ icon iconConfig "help" ]
                    , text "Measures"
                    ]
                , listItemDivider listItemDividerConfig
                , listItem
                    { listItemConfig
                        | onClick = Just (config.onNavItemClick Settings)
                    }
                    [ listItemGraphic [] [ icon iconConfig "settings" ]
                    , text "Settings"
                    ]
                ]
            ]
        ]


appBar : Session -> List String -> Html msg
appBar session title =
    topAppBar
        { topAppBarConfig
            | fixed = True
            , additionalAttributes = [ class "app-bar" ]
        }
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
