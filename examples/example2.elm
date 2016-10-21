module Main exposing(..)

import Date exposing (Date)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Value, (:=))
import Json.Decode as Json
import Task

import Gum exposing (..)

main = App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub (Msg Model)
subscriptions model = Sub.none

init : (Model, Cmd (Msg Model))
init = 
  ( { item = Nothing
    , log = []
    }
  , Cmd.none
  )

type alias Model = 
  { item : Maybe Item
  , log : List String
  }

type alias Item =
  { summary : String
  , version : String
  }

decodeItem : Json.Decoder Item
decodeItem =
  Json.object2 Item 
    ("summary" := Json.string)
    ("version" := Json.string)

view : Model -> Html (Msg Model)
view model =
  div
    []
    [ button 
        [ onClick <| 
            Call <| task Date.now <| \result ->
              case result of
                Err e ->
                  End 
                Ok d ->
                  log ("start loading at " ++ toString d) <|
                  load decodeItem "elm-package.json" <| \result ->
                    case result of
                      Err e ->
                        log ("loading failed") <|
                        End 
                      Ok value ->
                        Call <| task Date.now <| \result ->
                          case result of
                            Err e ->
                              End 
                            Ok d ->
                              log ("loading succeeded at " ++ toString d) <|
                              Poke (\m -> {m | item = Just value}) End
        ] 
        [ text "load"
        ]
    , text (toString model)
    ]


log : String -> Msg Model -> Msg Model
log s = Poke (\m -> { m | log = s :: m.log } )

load : Json.Decoder a -> String -> (Result Http.Error a -> Msg model) -> Msg model
load decoder url f =
  Call <| task (Http.get decoder url) f


