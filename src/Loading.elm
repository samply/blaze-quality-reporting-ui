module Loading exposing
    ( Status(..)
    , andThen
    , hasData
    , map
    , markLoadingSlowly
    , markReloading
    , slowThreshold
    )

import Fhir.Http exposing (Error)
import Process
import Task exposing (Task)


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Reloading a
    | ReloadingSlowly a
    | Failed Error


hasData : Status a -> Bool
hasData status =
    case status of
        Loaded _ ->
            True

        Reloading _ ->
            True

        ReloadingSlowly _ ->
            True

        _ ->
            False


map : (a -> b) -> Status a -> Maybe b
map f status =
    case status of
        Loaded x ->
            Just (f x)

        Reloading x ->
            Just (f x)

        ReloadingSlowly x ->
            Just (f x)

        _ ->
            Nothing


andThen : (a -> Maybe b) -> Status a -> Maybe b
andThen f status =
    case status of
        Loaded x ->
            f x

        Reloading x ->
            f x

        ReloadingSlowly x ->
            f x

        _ ->
            Nothing


slowThreshold : msg -> Cmd msg
slowThreshold msg =
    Task.perform (\_ -> msg) (Process.sleep 500)


markLoadingSlowly : Status a -> Status a
markLoadingSlowly status =
    case status of
        Loading ->
            LoadingSlowly

        Reloading x ->
            ReloadingSlowly x

        _ ->
            status


markReloading : Status a -> Status a
markReloading status =
    case status of
        Loaded x ->
            Reloading x

        _ ->
            status
