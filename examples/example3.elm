module Main exposing(..)

import Date exposing (Date)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Value, (:=))
import Json.Decode as Json
import Task

import Gum.Msg exposing (..)
import Gum.Action exposing (..)

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
            runAction <|
              liftTask Date.now >>= \result -> 
              case result of
                Err e ->
                  pureAction () 
                Ok d ->
                  log ("start loading on " ++ toString d) >>= \_ -> 
                  load decodeItem "elm-package.json" >>= \result ->
                    case result of
                      Err e ->
                        log ("loading failed")
                      Ok value ->
                        liftTask Date.now >>= \result ->
                          case result of
                            Err e ->
                              pureAction ()
                            Ok d ->
                              log ("loading succeeded on " ++ toString d) >>= \_ ->
                              updateModel (\m -> {m | item = Just value})
        ] 
        [ text "load"
        ]
    , text (toString model)
    ]


log : String -> Action Model ()
log s = updateModel (\m -> { m | log = s :: m.log } )

load : Json.Decoder a -> String -> Action Model (Result Http.Error a)
load decoder url =
  liftTask (Http.get decoder url)


