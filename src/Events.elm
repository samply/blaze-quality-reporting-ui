module Events exposing (onEnter, onEnterEsc, onEsc)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Decode


onEnterEsc : msg -> msg -> Attribute msg
onEnterEsc enterMsg escMsg =
    let
        withCode code =
            case code of
                13 ->
                    Decode.succeed enterMsg

                27 ->
                    Decode.succeed escMsg

                _ ->
                    Decode.fail "???"
    in
    on "keyup" (Decode.andThen withCode keyCode)


onEnter : msg -> Attribute msg
onEnter msg =
    on "keyup" (Decode.andThen (isCode 13 msg) keyCode)


onEsc : msg -> Attribute msg
onEsc msg =
    on "keyup" (Decode.andThen (isCode 27 msg) keyCode)


isCode expected msg code =
    if code == expected then
        Decode.succeed msg

    else
        Decode.fail "???"
