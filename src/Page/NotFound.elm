module Page.NotFound exposing (view)

import Html exposing (Html, text)


view : { title : List String, content : Html msg }
view =
    { title = [ "Page Not Found" ]
    , content =
        text "Not Found"
    }
