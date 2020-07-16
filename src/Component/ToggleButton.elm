module Component.ToggleButton exposing
    ( Config
    , config
    , setAttributes
    , setDisabled
    , setOn
    , setOnClick
    , text
    )

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events


type alias Config msg =
    { disabled : Bool
    , on : Bool
    , onClick : Maybe msg
    , additionalAttributes : List (Html.Attribute msg)
    }


{-| Default configuration of a button
-}
config : Config msg
config =
    { disabled = False
    , on = True
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


{-| Specify whether a toggle button is on
-}
setOn : Bool -> Config msg -> Config msg
setOn on config_ =
    { config_ | on = on }


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


{-| Text button variant (flush without outline)
-}
text : Config msg -> List (Html msg) -> Html msg
text ({ on, additionalAttributes } as config_) label =
    Html.button
        ([ class "text-sm font-medium"
         , if on then
            class "text-gray-800 hover:text-gray-900"

           else
            class "text-gray-600 hover:text-gray-900"
         , class "focus:outline-none"
         ]
            ++ List.filterMap identity
                [ disabledAttr config_
                , clickHandler config_
                ]
            ++ additionalAttributes
        )
        label


disabledAttr : Config msg -> Maybe (Html.Attribute msg)
disabledAttr { disabled } =
    Just (Html.Attributes.disabled disabled)


clickHandler : Config msg -> Maybe (Html.Attribute msg)
clickHandler { onClick } =
    Maybe.map Html.Events.onClick onClick
