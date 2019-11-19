module Component.Sidebar exposing (..)

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Material.Button exposing (buttonConfig, textButton)


type alias SidebarConfig msg =
    { additionalAttributes : List (Html.Attribute msg) }


sidebarConfig : SidebarConfig msg
sidebarConfig =
    { additionalAttributes = [] }


sidebar : SidebarConfig msg -> List (SidebarEntry msg) -> Html msg
sidebar config entries =
    div (class "sidebar" :: config.additionalAttributes)
        (List.map
            (\entry ->
                case entry of
                    SidebarEntry { node } ->
                        node
            )
            entries
        )


type SidebarEntry msg
    = SidebarEntry
        { config : SidebarConfig msg
        , node : Html msg
        }


type alias SidebarEntryConfig msg =
    { additionalAttributes : List (Html.Attribute msg) }


sidebarEntryConfig : SidebarEntryConfig msg
sidebarEntryConfig =
    { additionalAttributes = [] }


sidebarEntry : SidebarEntryConfig msg -> List (Html msg) -> SidebarEntry msg
sidebarEntry config nodes =
    SidebarEntry
        { config = config
        , node =
            div (class "sidebar-entry" :: config.additionalAttributes)
                nodes
        }


sidebarEntryTitle : List (Attribute msg) -> List (Html msg) -> Html msg
sidebarEntryTitle additionalAttributes nodes =
    Html.div
        (class "sidebar-entry__title mdc-typography--subtitle1"
            :: additionalAttributes
        )
        nodes


sidebarEntryContent : List (Attribute msg) -> List (Html msg) -> Html msg
sidebarEntryContent additionalAttributes nodes =
    Html.div
        (class "sidebar-entry__content mdc-typography--body2"
            :: additionalAttributes
        )
        nodes


type alias SidebarEditButtonConfig msg =
    { onClick : Maybe msg
    , additionalAttributes : List (Html.Attribute msg)
    }


sidebarEditButtonConfig =
    { onClick = Nothing
    , additionalAttributes = []
    }


sidebarEditButton : SidebarEditButtonConfig msg -> Html msg
sidebarEditButton config =
    textButton
        { buttonConfig
            | dense = True
            , onClick = config.onClick
            , additionalAttributes = config.additionalAttributes
        }
        "edit"
