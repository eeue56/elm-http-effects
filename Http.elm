effect module Http where { command = MyCmd, subscription = MySub } exposing (..)

import Json.Decode as Json
import Task exposing (Task)
import Dict exposing (Dict)
import LowHttp exposing (..)

type Method 
    = Get
    | Post 
    | Put 
    | Delete



methodToString : Method -> String 
methodToString = toString

toDictTuple : Method -> Url -> (String, String) 
toDictTuple method url =
    (methodToString method, url) 



type MyCmd msg
  = Send Method Url Json.Value
  | SendAndSub Method Url Json.Value (Json.Value -> msg)


send : Method -> Url -> Json.Value -> Cmd msg
send method url value = 
    command (Send method url value)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
  case cmd of
    (Send method url value) -> 
      Send method url value

    (SendAndSub method url value tagger) -> 
      SendAndSub method url value (tagger >> f)

type MySub msg
  = Listen Method Url (Json.Value -> msg)
  | All 


listen : Method -> Url -> (Json.Value -> msg) -> Sub msg 
listen method url tagger =
    subscription (Listen method url tagger)

listenToAll : Sub msg 
listenToAll =
    subscription All

sendAndListen : Method -> Url -> Json.Value -> (Json.Value -> msg) -> (Cmd msg, Sub msg)
sendAndListen method url value tagger =
    ( send method url value
    , listen method url tagger 
    )

sendAndSub : Method -> Url -> Json.Value -> (Json.Value -> msg) -> Cmd msg
sendAndSub method url value tagger =
    command (SendAndSub method url value tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
  case sub of
    Listen method url tagger ->
      Listen method url (tagger >> func)
    All ->
      All


-- MANAGER


type alias State msg =
  { subs : SubsDict msg
  }


type alias SubsDict msg =
  Dict.Dict (String, String) (List (Json.Value -> msg))



init : Task Never (State msg)
init =
  Task.succeed (State Dict.empty)



-- HANDLE APP MESSAGES


(&>) t1 t2 = Task.andThen t1 (\_ -> t2)


onEffects
  : Platform.Router msg (Msg)
  -> List (MyCmd msg)
  -> List (MySub msg)
  -> State msg
  -> Task Never (State msg)
onEffects router cmds subs state =
  let
    sendMessagesGetNewQueues =
      sendMessagesHelp router cmds

    subsFromCmds = 
      getNewSubs cmds

    newSubs =
      buildSubDict (subs ++ subsFromCmds) Dict.empty
  in
    sendMessagesGetNewQueues 
        |> Task.map (\nothing -> { subs = newSubs })


getNewSubs : List (MyCmd msg) -> List (MySub msg)
getNewSubs cmds =
    let 

      getNewSub cmd =
        case cmd of
          SendAndSub method url msg tagger -> 
            Just (Listen method url tagger)   
          _ -> 
            Nothing
    in
      List.filterMap getNewSub cmds



sendMessagesHelp : Platform.Router msg Msg -> List (MyCmd msg) -> Task x ()
sendMessagesHelp router cmds =
    case cmds of
        [] ->
            Task.succeed () 

        Send method url msg :: rest ->
            LowHttp.send 
                (methodToString method) 
                url  
                msg
                { onMessage = \method name msg -> Platform.sendToSelf router (Receive (method, name) msg) }
                &> (sendMessagesHelp router rest)

        SendAndSub method url msg tagger :: rest ->
            LowHttp.send 
                (methodToString method)
                url 
                msg 
                { onMessage = \method name msg -> Platform.sendToSelf router (Receive (method, name) msg) }
                &> (sendMessagesHelp router rest)


    

{-| adds a new sub to the dict of subs
-}
buildSubDict : List (MySub msg) -> SubsDict msg -> SubsDict msg
buildSubDict subs dict =
  case subs of
    [] ->
      dict

    Listen method url tagger :: rest ->
      buildSubDict rest (Dict.update (toDictTuple method url) (add tagger) dict)

    All :: rest ->
      buildSubDict rest dict


add : a -> Maybe (List a) -> Maybe (List a)
add value maybeList =
  case maybeList of
    Nothing ->
      Just [value]

    Just list ->
      Just (value :: list)


-- HANDLE SELF MESSAGES


type Msg 
  = Receive (String, String) Json.Value


onSelfMsg : Platform.Router msg (Msg) -> (Msg) -> State msg -> Task x (State msg)
onSelfMsg router selfMsg state =
  case selfMsg of
    Receive (method, url) msg ->
      let
        sends =
          Dict.get (method, url) state.subs
            |> Maybe.withDefault []
            |> List.map (\tagger -> Platform.sendToApp router (tagger msg))
      in
        Task.sequence sends &> Task.succeed state

