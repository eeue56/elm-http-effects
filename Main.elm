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


type Action 
    = LoadUsers (List User)
    | GetUsers 

update : Action -> Model -> (Model, Cmd Action)
update action model = 
    case action of
        LoadUsers users ->
            ( { model | users = users }, Cmd.none )

        GetUsers ->
            ( model, Http.send Http.Get "/users" (Json.Encode.string "") )



decodeUsers : Json.Decode.Decoder (List User)
decodeUsers =
    Json.Decode.list Json.Decode.string


loadUsers : Json.Decode.Value -> Action
loadUsers value = 
    case Json.Decode.decodeValue decodeUsers value of
        Err _ ->
            LoadUsers [ "wrong" ]
        Ok users ->
            LoadUsers users


subs : Model -> Sub Action
subs model = 
    Sub.batch
        [ Http.listen Http.Get "/users" loadUsers
        ]


view : Model -> Html Action 
view model = 
    div 
        []
        ( [ div [ onClick GetUsers ] [ text "click to get users" ] ] ++ 
            List.map (\user -> text user) model.users
        )