module Page.Measure.List.Query exposing (Query)

import Fhir.ValueSet.PublicationStatus exposing (PublicationStatus)
import Page.Measure.List.Sort exposing (Sort)


type alias Query =
    { status : PublicationStatus
    , title : Maybe String
    , sort : Sort
    }
