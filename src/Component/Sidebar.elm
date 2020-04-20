module Component.Sidebar exposing (Config, config, setAttributes, view)

import Component.Sidebar.Entry.Internal exposing (SidebarEntry(..))
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)


type Config msg
    = Config { additionalAttributes : List (Attribute msg) }


config : Config msg
config =
    Config { additionalAttributes = [] }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


view : Config msg -> List (SidebarEntry msg) -> Html msg
view (Config { additionalAttributes }) entries =
    div (class "sidebar" :: additionalAttributes)
        (List.map
            (\(SidebarEntry { node }) -> node)
            entries
        )
