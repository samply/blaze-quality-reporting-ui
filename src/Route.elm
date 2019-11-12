module Route exposing (Route(..), fromUrl, href, pushUrl)

import Browser.Navigation as Nav
import Fhir.PrimitiveTypes exposing (Id, Uri)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
    = MeasureList
    | Measure Id
    | LibraryByUrl Uri


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map MeasureList Parser.top
        , Parser.map Measure (s "measure" </> string)
        , Parser.map LibraryByUrl (s "library-by-uri" </> string)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                MeasureList ->
                    []

                Measure id ->
                    [ "measure", id ]

                LibraryByUrl uri ->
                    [ "library-by-uri", uri ]
    in
    "#/" ++ String.join "/" pieces
