module Component.Radio exposing
    ( Config
    , config
    , radio
    , setAttributes
    , setChecked
    , setDisabled
    , setOnChange
    )

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events


type alias Config msg =
    { checked : Bool
    , disabled : Bool
    , additionalAttributes : List (Html.Attribute msg)
    , onChange : Maybe msg
    }


{-| Default radio button configuration
-}
config : Config msg
config =
    { checked = False
    , disabled = False
    , additionalAttributes = []
    , onChange = Nothing
    }


{-| Specify whether a radio button is checked
-}
setChecked : Bool -> Config msg -> Config msg
setChecked checked config_ =
    { config_ | checked = checked }


{-| Specify whether a radio button is disabled

Disabled radio buttons cannot be interacted with and have no visual interaction
effect.

-}
setDisabled : Bool -> Config msg -> Config msg
setDisabled disabled config_ =
    { config_ | disabled = disabled }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user changes a radio
-}
setOnChange : msg -> Config msg -> Config msg
setOnChange onChange config_ =
    { config_ | onChange = Just onChange }


{-| Radio button view function
-}
radio : Config msg -> Html msg
radio ({ additionalAttributes } as config_) =
    Html.input
        ([ Html.Attributes.type_ "radio" ]
            ++ List.filterMap identity
                [ checkedProp config_
                , disabledProp config_
                , changeHandler config_
                ]
            ++ additionalAttributes
        )
        []


checkedProp : Config msg -> Maybe (Html.Attribute msg)
checkedProp { checked } =
    Just (Html.Attributes.checked checked)


disabledProp : Config msg -> Maybe (Html.Attribute msg)
disabledProp { disabled } =
    Just (Html.Attributes.disabled disabled)


changeHandler : Config msg -> Maybe (Html.Attribute msg)
changeHandler { onChange } =
    Maybe.map (\msg -> Html.Events.onCheck (\_ -> msg)) onChange
