module Session exposing
    ( Server
    , Session
    , activeServer
    , decoder
    , default
    , emptyServer
    , encode
    , getBase
    )

import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Decode.Zipper exposing (zipper)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Zipper as EncodeZipper
import List.Zipper as Zipper exposing (Zipper)
import Time


type alias Session =
    { timeZone : Time.Zone
    , servers : Zipper Server
    }


type alias Server =
    { name : String
    , url : String
    }


default : Session
default =
    { timeZone = Time.utc
    , servers =
        Zipper.fromCons
            { name = "Localhost 8080"
            , url = "http://localhost:8080/fhir"
            }
            []
    }


{-| Returns the base URL of the FHIR server to talk to.
-}
getBase : Session -> String
getBase session =
    (activeServer session).url


activeServer : Session -> Server
activeServer session =
    Zipper.current session.servers


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


decoder : Decoder Session
decoder =
    succeed Session
        |> hardcoded Time.utc
        |> required "servers" (zipper serverDecoder)


serverDecoder : Decoder Server
serverDecoder =
    succeed Server
        |> required "name" string
        |> required "url" string
