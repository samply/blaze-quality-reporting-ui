module Session exposing (Session, navKey)

import Browser.Navigation as Nav


type alias Session =
    { navKey : Nav.Key
    , base : String
    }


navKey : Session -> Nav.Key
navKey session =
    session.navKey
