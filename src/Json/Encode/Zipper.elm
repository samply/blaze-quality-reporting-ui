module Json.Encode.Zipper exposing (zipper)

import Json.Encode as Encode exposing (Value)
import List.Zipper as Zipper exposing (Zipper)


zipper : (a -> Value) -> Zipper a -> Value
zipper f entries =
    Encode.object
        [ ( "before", Encode.list f (Zipper.before entries) )
        , ( "current", f (Zipper.current entries) )
        , ( "after", Encode.list f (Zipper.after entries) )
        ]
