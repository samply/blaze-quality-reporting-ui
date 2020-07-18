module Component.List exposing
    ( Config
    , config
    , list
    , setAttributes
    , setDense
    , setNonInteractive
    , setTopBorder
    )

import Component.List.Item exposing (ListItem)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed


type alias Config msg =
    { topBorder : Bool
    , dense : Bool
    , nonInteractive : Bool
    , additionalAttributes : List (Html.Attribute msg)
    }


{-| Default configuration of a button
-}
config : Config msg
config =
    { topBorder = True
    , dense = False
    , nonInteractive = False
    , additionalAttributes = []
    }


setTopBorder : Bool -> Config msg -> Config msg
setTopBorder topBorder config_ =
    { config_ | topBorder = topBorder }


{-| Specify whether a list item is _dense_

Dense list items feature smaller than normal padding.

-}
setDense : Bool -> Config msg -> Config msg
setDense dense config_ =
    { config_ | dense = dense }


{-| Specify whether a list should be non-interactive

Non-interactive lists do not feature keyboard interaction and list items have
no visual interaction effect.

-}
setNonInteractive : Bool -> Config msg -> Config msg
setNonInteractive nonInteractive config_ =
    { config_ | nonInteractive = nonInteractive }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    { config_ | additionalAttributes = additionalAttributes }


list : Config msg -> List (ListItem msg) -> Html msg
list ({ topBorder, additionalAttributes } as config_) listItems =
    Keyed.ul
        ([ class "overflow-hidden border border-gray-400 rounded-md"
         , class "bg-white"
         , if topBorder then
            class ""

           else
            class "border-t-0 rounded-t-none"
         ]
            ++ additionalAttributes
        )
        (List.map (listItemView config_) listItems)


listItemView : Config msg -> ListItem msg -> ( String, Html msg )
listItemView { dense, nonInteractive } { key, attributes, content } =
    ( key
    , Html.li
        ([ class "block border-b border-gray-400 last:border-b-0"
         , if dense then
            class "px-4 py-2"

           else
            class "p-4"
         , class "text-gray-900"
         , if nonInteractive then
            class ""

           else
            class "cursor-pointer hover:bg-gray-200"
         ]
            ++ attributes
        )
        content
    )
