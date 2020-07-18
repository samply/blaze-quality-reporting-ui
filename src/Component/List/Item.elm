module Component.List.Item exposing
    ( Config
    , ListItem
    , config
    , graphic
    , listItem
    , meta
    , setAttributes
    , setDisabled
    , setOnClick
    , text
    )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)
import Html.Events


type alias Config msg =
    { disabled : Bool
    , onClick : Maybe msg
    , additionalAttributes : List (Html.Attribute msg)
    }


{-| Default configuration of a button
-}
config : Config msg
config =
    { disabled = False
    , onClick = Nothing
    , additionalAttributes = []
    }


{-| Specify whether a list item should be disabled

Disabled list items cannot be interacted with and have not visual interaction
effect.

-}
setDisabled : Bool -> Config msg -> Config msg
setDisabled disabled config_ =
    { config_ | disabled = disabled }


{-| Specify a message when the user clicks a list item
-}
setOnClick : msg -> Config msg -> Config msg
setOnClick onClick config_ =
    { config_ | onClick = Just onClick }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    { config_ | additionalAttributes = additionalAttributes }


type alias ListItem msg =
    { key : String
    , attributes : List (Attribute msg)
    , content : List (Html msg)
    }


listItem : Config msg -> String -> List (Html msg) -> ListItem msg
listItem ({ additionalAttributes } as config_) key content =
    { key = key
    , attributes =
        [ class "flex" ]
            ++ List.filterMap identity
                [ clickHandler config_
                ]
            ++ additionalAttributes
    , content = content
    }


clickHandler : Config msg -> Maybe (Html.Attribute msg)
clickHandler { onClick } =
    Maybe.map Html.Events.onClick onClick


{-| Two-line list item's text
-}
text :
    List (Html.Attribute msg)
    ->
        { primary : List (Html msg)
        , secondary : List (Html msg)
        }
    -> Html msg
text additionalAttributes { primary, secondary } =
    Html.div (class "" :: additionalAttributes)
        [ primaryText [] primary
        , secondaryText [] secondary
        ]


primaryText : List (Html.Attribute msg) -> List (Html msg) -> Html msg
primaryText additionalAttributes nodes =
    Html.div (class "" :: additionalAttributes) nodes


secondaryText : List (Html.Attribute msg) -> List (Html msg) -> Html msg
secondaryText additionalAttributes nodes =
    Html.div (class "text-gray-600 text-sm" :: additionalAttributes) nodes


{-| A list item's graphic tile
-}
graphic : List (Html.Attribute msg) -> List (Html msg) -> Html msg
graphic additionalAttributes nodes =
    Html.div (class "ml-0 mr-8 w-6 h-6 items-center justify-center" :: additionalAttributes) nodes


{-| A list item's meta tile
-}
meta : List (Html.Attribute msg) -> List (Html msg) -> Html msg
meta additionalAttributes nodes =
    Html.div (class "ml-auto mr-0 h-6" :: additionalAttributes) nodes
