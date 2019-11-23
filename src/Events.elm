module Events exposing (onEnter)

import Html exposing (Attribute, Html)
import Html.Events exposing (keyCode, on)
import Json.Decode as Decode


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keyup" (Decode.andThen isEnter keyCode)
