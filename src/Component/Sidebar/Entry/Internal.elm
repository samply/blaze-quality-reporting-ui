module Component.Sidebar.Entry.Internal exposing (Config(..), SidebarEntry(..))

import Html exposing (Attribute, Html)


type Config msg
    = Config { additionalAttributes : List (Attribute msg) }


type SidebarEntry msg
    = SidebarEntry
        { config : Config msg
        , node : Html msg
        }
