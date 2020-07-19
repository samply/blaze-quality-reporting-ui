module Route exposing (Route(..))

import Fhir.PrimitiveTypes exposing (Id)


type Route
    = LibraryList
    | Library Id
    | MeasureList
    | Measure Id
    | MeasureReport Id
    | Settings
    | Help
