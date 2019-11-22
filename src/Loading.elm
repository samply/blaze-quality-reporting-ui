module Loading exposing (Status(..))

import Fhir.Http exposing (Error)


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed Error
