module Util exposing (TimeCtx, applyIf, emptyToNothing, liftMaybe)

import Time


applyIf : Bool -> (a -> a) -> a -> a
applyIf apply f config =
    if apply then
        f config

    else
        config


type alias TimeCtx =
    { timeZone : Time.Zone
    , currentTime : Time.Posix
    }


liftMaybe : (a -> config -> config) -> (Maybe a -> config -> config)
liftMaybe setter =
    \maybeValue config ->
        case maybeValue of
            Just value ->
                setter value config

            Nothing ->
                config


emptyToNothing : String -> Maybe String
emptyToNothing s =
    if String.isEmpty s then
        Nothing

    else
        Just s
