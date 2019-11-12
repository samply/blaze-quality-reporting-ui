module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, h3, h6, text)
import Html.Attributes exposing (class, style)
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


type NavItem
    = Measures


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
    -> Bool
    -> { title : List String, content : Html pageMsg }
    -> Document msg
view toPageMsg config drawerOpen { title, content } =
    { title =
        "Blaze CDS"
            :: title
            |> List.reverse
            |> String.join " - "
    , body =
        [ div
            [ Typography.typography
            , style "display" "flex"
            , style "align-items" "stretch"
            , style "height" "100vh"
            ]
            [ drawer config drawerOpen
            , div [ class "content" ]
                [ appBar config.onNavIconClick title
                , div [ class "main-content mdc-top-app-bar--fixed-adjust" ]
                    [ Html.map toPageMsg content ]
                ]
            ]
        ]
    }


drawer config drawerOpen =
    permanentDrawer
        { permanentDrawerConfig | additionalAttributes = [ class "drawer" ] }
        [ drawerHeader []
            [ h3 [ drawerTitle ] [ text "Blaze CDS" ]
            , h6 [ drawerSubtitle ]
                [ text "Clinical Decision Support" ]
            ]
        , drawerContent [ class "drawer__content" ]
            [ list listConfig
                [ listItem
                    { listItemConfig
                        | onClick = Just (config.onNavItemClick Measures)
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
                , listItem
                    { listItemConfig
                        | onClick = Just (config.onNavItemClick Measures)
                    }
                    [ listItemGraphic [] [ icon iconConfig "info" ]
                    , text "Measure Reports"
                    ]
                , listItemDivider listItemDividerConfig
                , listItem
                    { listItemConfig
                        | onClick = Just (config.onNavItemClick Measures)
                    }
                    [ listItemGraphic [] [ icon iconConfig "settings" ]
                    , text "Settings"
                    ]
                ]
            ]
        ]


appBar : msg -> List String -> Html msg
appBar onNavIconClick title =
    topAppBar { topAppBarConfig | fixed = True }
        [ TopAppBar.row []
            [ TopAppBar.section [ TopAppBar.alignStart ]
                [ Html.span [ TopAppBar.title ]
                    [ text (String.join " " title) ]
                ]
            ]
        ]
