module LowHttp exposing (..)

import Json.Decode as Json 
import Task exposing (Task)
import Native.Http 

type alias Method = String 

type alias Url = String 

type alias Settings = 
    { onMessage : Method -> Url -> Json.Value -> Task Never () 
    }

send : String -> Url -> Json.Value -> Settings -> Task x ()
send method url value settings =
    Native.Http.send method url value settings


request : String -> Url -> Task x Json.Value
request method url = 
    Native.Http.request method url 

listen : String -> Url -> (Json.Value -> msg) -> Task x ((String, String), msg)
listen method url tagger = 
    request method url 
        |> Task.map tagger
        |> Task.map (\value -> ((method, url), value))