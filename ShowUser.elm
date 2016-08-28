module ShowUser exposing (..)


import Html exposing (Html, text, div)
import Html.Events exposing (onClick)
import Html.App
import Http
import Json.Decode
import Json.Encode



type alias User = String

type alias Model = 
    String


type Action 
    = LoadName String
    | NoOp


update : Action -> Model -> (Model, Cmd Action)
update action model = 
    case action of
        LoadName name ->
            (name, Cmd.none)
        NoOp ->
            (model, Cmd.none)



loadName : Json.Decode.Value -> Action
loadName value = 
    case Json.Decode.decodeValue Json.Decode.string value of
        Err msg ->
            NoOp
        Ok name ->
            LoadName name


subs : Model -> Sub Action
subs model = 
    [ Http.listen Http.Get "/dave" loadName
    ]
        |> Sub.batch


view : Model -> Html Action 
view model = 
    div 
        []
        [ text ("name is " ++ model) ]