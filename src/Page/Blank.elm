module Page.Blank exposing (view)

import Html exposing (Html)


view : { title : List String, content : Html msg }
view =
    { title = [ "" ]
    , content = Html.text ""
    }
