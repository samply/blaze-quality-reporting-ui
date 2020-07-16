module Component.Button exposing
    ( Config
    , config
    , create
    , icon
    , primary
    , secondary
    , setAttributes
    , setDisabled
    , setOnClick
    , text
    )

import Html exposing (Html)
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


{-| Specify whether the button is disabled

Disabled buttons cannot be interacted with and do not have no visual
interaction effect.

-}
setDisabled : Bool -> Config msg -> Config msg
setDisabled disabled config_ =
    { config_ | disabled = disabled }


{-| Specify a message when the user clicks a button
-}
setOnClick : msg -> Config msg -> Config msg
setOnClick onClick config_ =
    { config_ | onClick = Just onClick }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    { config_ | additionalAttributes = additionalAttributes }


{-| Primary button variant
-}
primary : Config msg -> String -> Html msg
primary ({ disabled, additionalAttributes } as config_) label =
    Html.button
        ([ class "text-sm text-white font-semibold"
         , class "whitespace-no-wrap"
         , class "border rounded-md px-2 py-1"
         , if disabled then
            class "bg-gray-600 border-gray-600 cursor-default"

           else
            class "bg-blue-600 border-blue-600 hover:bg-blue-700 focus:border-blue-900"
         , class "focus:outline-none"
         ]
            ++ List.filterMap identity
                [ disabledAttr config_
                , clickHandler config_
                ]
            ++ additionalAttributes
        )
        [ Html.text label ]


{-| Secondary button variant
-}
secondary : Config msg -> String -> Html msg
secondary ({ disabled, additionalAttributes } as config_) label =
    Html.button
        ([ class "text-sm font-medium"
         , class "whitespace-no-wrap"
         , class "border rounded-md px-2 py-1"
         , if disabled then
            class "border-gray-600 text-gary-600 cursor-default"

           else
            class "text-blue-600 border-blue-600 hover:text-blue-800 hover:border-blue-800 focus:border-blue-900"
         , class "focus:outline-none"
         ]
            ++ List.filterMap identity
                [ disabledAttr config_
                , clickHandler config_
                ]
            ++ additionalAttributes
        )
        [ Html.text label ]


{-| Create button variant
-}
create : Config msg -> String -> Html msg
create ({ disabled, additionalAttributes } as config_) label =
    Html.button
        ([ class "text-sm text-white font-semibold"
         , class "whitespace-no-wrap"
         , class "border rounded-md p-2"
         , if disabled then
            class "bg-gray-600 border-gray-600 cursor-default"

           else
            class "bg-green-600 border-green-600 hover:bg-green-700 focus:border-green-900"
         , class "focus:outline-none"
         ]
            ++ List.filterMap identity
                [ disabledAttr config_
                , clickHandler config_
                ]
            ++ additionalAttributes
        )
        [ Html.text label ]


{-| Text button variant (flush without outline)
-}
text : Config msg -> String -> Html msg
text ({ disabled, additionalAttributes } as config_) label =
    Html.button
        ([ class "text-sm font-medium"
         , if disabled then
            class "text-gray-600 cursor-default"

           else
            class "text-blue-600 hover:text-blue-800"
         , class "uppercase tracking-wide"
         , class "focus:outline-none"
         ]
            ++ List.filterMap identity
                [ disabledAttr config_
                , clickHandler config_
                ]
            ++ additionalAttributes
        )
        [ Html.text label ]


{-| Icon button variant
-}
icon : Config msg -> String -> Html msg
icon ({ disabled, additionalAttributes } as config_) label =
    Html.button
        ([ class "material-icons"
         , if disabled then
            class "text-gray-600 cursor-default"

           else
            class "text-gray-600 hover:text-gray-800"
         , class "focus:outline-none"
         ]
            ++ List.filterMap identity
                [ disabledAttr config_
                , clickHandler config_
                ]
            ++ additionalAttributes
        )
        [ Html.text label ]


disabledAttr : Config msg -> Maybe (Html.Attribute msg)
disabledAttr { disabled } =
    Just (Html.Attributes.disabled disabled)


clickHandler : Config msg -> Maybe (Html.Attribute msg)
clickHandler { onClick } =
    Maybe.map Html.Events.onClick onClick
