module Main exposing (..)


import Html exposing (Html, text, div)
import Html.Events exposing (onClick)
import Html.App
import Http
import Json.Decode
import Json.Encode


main : Program Never
main = 
    Html.App.program 
        { init = ({ users = []}, Cmd.none) 
        , update = update
        , view = view
        , subscriptions = subs
        }


type alias User = String

type alias Model = 
    { users : List String }

getAndRegisterDave : Cmd Action
getAndRegisterDave = 
    Http.sendAndSub Http.Get "/dave" (Json.Encode.string "") loadDave


type Action 
    = LoadUsers (List User)
    | GetUsers 
    | GetDave
    | GetDaveWithAutoSub
    | FailedToLoad String


update : Action -> Model -> (Model, Cmd Action)
update action model = 
    case action of
        LoadUsers users ->
            ( { model | users = model.users ++ users }, Cmd.none )

        GetUsers ->
            ( model, Http.sendAndSub Http.Get "/users" (Json.Encode.string "") loadUsers )

        GetDave ->
            ( model, Http.sendAndSub Http.Get "/dave" (Json.Encode.string "") loadDave )

        GetDaveWithAutoSub ->
            ( model, getAndRegisterDave )

        FailedToLoad msg ->
            (model, Cmd.none)



decodeUsers : Json.Decode.Decoder (List User)
decodeUsers =
    Json.Decode.list Json.Decode.string


loadUsers : Json.Decode.Value -> Action
loadUsers value = 
    case Json.Decode.decodeValue decodeUsers value of
        Err msg ->
            FailedToLoad msg
        Ok users ->
            LoadUsers users

loadDave : Json.Decode.Value -> Action 
loadDave value = 
    case Json.Decode.decodeValue Json.Decode.string value of
        Err msg ->
            FailedToLoad msg
        Ok dave ->
            LoadUsers [ dave ]


subs : Model -> Sub Action
subs model = 
    [ Http.listenToAll
    ]
        |> Sub.batch


view : Model -> Html Action 
view model = 
    div 
        []
        ( [ div [ onClick GetUsers ] [ text "click to get users" ]
          , div [ onClick GetDave ] [ text "click to get dave" ] 
          , div [ onClick GetDaveWithAutoSub ] [ text "click to get dave with auto sub" ] 
          ] 
            ++ 
            List.map (\user -> text user) model.users
        )