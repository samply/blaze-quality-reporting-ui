module MaterialUtil exposing (liftMaybe)


liftMaybe : (a -> config -> config) -> (Maybe a -> config -> config)
liftMaybe setter =
    \maybeValue config ->
        case maybeValue of
            Just value ->
                setter value config

            Nothing ->
                config
