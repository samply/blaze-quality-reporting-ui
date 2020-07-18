module Component.TextField exposing
    ( Config
    , config
    , outlined
    , setAttributes
    , setDisabled
    , setId
    , setOnChange
    , setOnEnter
    , setOnEsc
    , setOnInput
    , setPlaceholder
    , setRequired
    , setValid
    , setValue
    )

import Events
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Json.Decode as Decode


type alias Config msg =
    { id : Maybe String
    , required : Bool
    , disabled : Bool
    , valid : Bool
    , placeholder : Maybe String
    , value : Maybe String
    , onInput : Maybe (String -> msg)
    , onChange : Maybe (String -> msg)
    , onEnter : Maybe msg
    , onEsc : Maybe msg
    , additionalAttributes : List (Html.Attribute msg)
    }


{-| Default configuration of a textField
-}
config : Config msg
config =
    { id = Nothing
    , required = False
    , disabled = False
    , valid = True
    , placeholder = Nothing
    , value = Nothing
    , onInput = Nothing
    , onChange = Nothing
    , onEnter = Nothing
    , onEsc = Nothing
    , additionalAttributes = []
    }


{-| Specify a text field's id
-}
setId : Maybe String -> Config msg -> Config msg
setId id config_ =
    { config_ | id = id }


{-| Specify a text field to be required
-}
setRequired : Bool -> Config msg -> Config msg
setRequired required config_ =
    { config_ | required = required }


{-| Specify a text field to be disabled
-}
setDisabled : Bool -> Config msg -> Config msg
setDisabled disabled config_ =
    { config_ | disabled = disabled }


{-| Specify a text field to be valid
-}
setValid : Bool -> Config msg -> Config msg
setValid valid config_ =
    { config_ | valid = valid }


{-| Specify a text field's placeholder
-}
setPlaceholder : Maybe String -> Config msg -> Config msg
setPlaceholder placeholder config_ =
    { config_ | placeholder = placeholder }


{-| Specify a text field's value
-}
setValue : Maybe String -> Config msg -> Config msg
setValue value config_ =
    { config_ | value = value }


{-| Specify a message when the user changes the value inside the text field
-}
setOnInput : (String -> msg) -> Config msg -> Config msg
setOnInput onInput config_ =
    { config_ | onInput = Just onInput }


{-| Specify a message when the user confirms a changed value inside the text
field
-}
setOnChange : (String -> msg) -> Config msg -> Config msg
setOnChange onChange config_ =
    { config_ | onChange = Just onChange }


{-| Specify a message when the user hits the enter key inside the text field
-}
setOnEnter : msg -> Config msg -> Config msg
setOnEnter onEnter config_ =
    { config_ | onEnter = Just onEnter }


{-| Specify a message when the user hits the esc key inside the text field
-}
setOnEsc : msg -> Config msg -> Config msg
setOnEsc onEsc config_ =
    { config_ | onEsc = Just onEsc }


{-| Specify additional attributes
-}
setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    { config_ | additionalAttributes = additionalAttributes }


{-| Text textField variant (flush without outline)
-}
outlined : Config msg -> Html msg
outlined ({ valid, additionalAttributes } as config_) =
    Html.input
        ([ class "appearance-none px-2 py-1 rounded-md w-full bg-white border"
         , if valid then
            class "border-gray-400 focus:border-blue-600"

           else
            class "border-red-400 focus:border-red-600"
         , class "focus:outline-none"
         ]
            ++ List.filterMap identity
                [ idAttr config_
                , requiredAttr config_
                , disabledAttr config_
                , placeholderAttr config_
                , valueAttr config_
                , inputHandler config_
                , changeHandler config_
                , enterEscHandler config_
                ]
            ++ additionalAttributes
        )
        []


idAttr : Config msg -> Maybe (Html.Attribute msg)
idAttr { id } =
    Maybe.map Html.Attributes.id id


requiredAttr : Config msg -> Maybe (Html.Attribute msg)
requiredAttr { required } =
    Just (Html.Attributes.required required)


disabledAttr : Config msg -> Maybe (Html.Attribute msg)
disabledAttr { disabled } =
    Just (Html.Attributes.disabled disabled)


placeholderAttr : Config msg -> Maybe (Html.Attribute msg)
placeholderAttr { placeholder } =
    Maybe.map Html.Attributes.placeholder placeholder


valueAttr : Config msg -> Maybe (Html.Attribute msg)
valueAttr { value } =
    Maybe.map Html.Attributes.value value


inputHandler : Config msg -> Maybe (Html.Attribute msg)
inputHandler { onInput } =
    Maybe.map Html.Events.onInput onInput


changeHandler : Config msg -> Maybe (Html.Attribute msg)
changeHandler { onChange } =
    Maybe.map (\f -> Html.Events.on "change" (Decode.map f Html.Events.targetValue))
        onChange


enterEscHandler : Config msg -> Maybe (Html.Attribute msg)
enterEscHandler { onEnter, onEsc } =
    Just (Events.onEnterEsc2 onEnter onEsc)
