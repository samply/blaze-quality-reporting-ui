module Component.Sidebar.Entry exposing
    ( Config
    , SidebarEntry
    , actionButtons
    , config
    , content
    , editButton
    , setAttributes
    , title
    , view
    )

import Component.Button as Button
import Component.Sidebar.Entry.Internal as SidebarEntry
    exposing
        ( Config(..)
        , SidebarEntry(..)
        )
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)


type alias SidebarEntry msg =
    SidebarEntry.SidebarEntry msg


type alias Config msg =
    SidebarEntry.Config msg


config : Config msg
config =
    Config { additionalAttributes = [] }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


view : Config msg -> List (Html msg) -> SidebarEntry msg
view ((Config { additionalAttributes }) as config_) nodes =
    SidebarEntry
        { config = config_
        , node =
            div (class "p-2 border-b border-gray-400" :: additionalAttributes)
                nodes
        }


title : List (Attribute msg) -> List (Html msg) -> Html msg
title additionalAttributes nodes =
    div
        (class "flex justify-between mb-2"
            :: additionalAttributes
        )
        nodes


content : List (Attribute msg) -> List (Html msg) -> Html msg
content additionalAttributes nodes =
    div
        (class "text-gray-700 text-sm mb-2"
            :: additionalAttributes
        )
        nodes


editButton : Button.Config msg -> Html msg
editButton config_ =
    Button.text config_ "edit"


actionButtons : List (Attribute msg) -> List (Html msg) -> Html msg
actionButtons additionalAttributes nodes =
    div (class "space-x-2" :: additionalAttributes) nodes
