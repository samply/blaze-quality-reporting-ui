module Page.Measure.Util exposing
    ( updateGroup
    , updatePopulation
    , updateStratifier
    )

import Fhir.Measure as Measure exposing (Measure)
import List.Extra exposing (updateAt)


updatePopulation :
    (Measure.Population -> Measure.Population)
    -> Int
    -> Measure.Group
    -> Measure.Group
updatePopulation f groupIdx group =
    { group | population = updateAt groupIdx f group.population }


updateStratifier :
    (Measure.Stratifier -> Measure.Stratifier)
    -> Int
    -> Measure.Group
    -> Measure.Group
updateStratifier f groupIdx group =
    { group | stratifier = updateAt groupIdx f group.stratifier }


updateGroup :
    (Int -> Measure.Group -> Measure.Group)
    -> Int
    -> Int
    -> Measure
    -> Measure
updateGroup f groupIdx idx measure =
    { measure | group = updateAt groupIdx (f idx) measure.group }
