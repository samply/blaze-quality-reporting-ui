module Session exposing
    ( Server
    , Session
    , activeServer
    , decoder
    , default
    , emptyServer
    , encode
    , getBase
    , toNavKey
    )

import Browser.Navigation as Nav
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Decode.Zipper exposing (zipper)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Zipper as EncodeZipper
import List.Zipper as Zipper exposing (Zipper)


type alias Session =
    { navKey : Nav.Key
    , servers : Zipper Server
    }


type alias Server =
    { name : String
    , url : String
    }


default : Nav.Key -> Session
default navKey =
    { navKey = navKey
    , servers =
        Zipper.fromCons
            { name = "Localhost"
            , url = "http://localhost:8080/fhir"
            }
            [ { name = "Blaze LIFE"
              , url = "https://blaze.life.uni-leipzig.de/fhir"
              }
            ]
    }


{-| Returns the base URL of the FHIR server to talk to.
-}
getBase : Session -> String
getBase session =
    (activeServer session).url


activeServer : Session -> Server
activeServer session =
    Zipper.current session.servers


toNavKey : Session -> Nav.Key
toNavKey session =
    session.navKey


emptyServer : Server
emptyServer =
    { name = ""
    , url = ""
    }


encode : Session -> Value
encode session =
    Encode.object
        [ ( "servers", EncodeZipper.zipper encodeServer session.servers ) ]


encodeServer : Server -> Value
encodeServer server =
    Encode.object
        [ ( "name", Encode.string server.name )
        , ( "url", Encode.string server.url )
        ]


decoder : Nav.Key -> Decoder Session
decoder navKey =
    succeed Session
        |> hardcoded navKey
        |> required "servers" (zipper serverDecoder)


serverDecoder : Decoder Server
serverDecoder =
    succeed Server
        |> required "name" string
        |> required "url" string
