module Main exposing(..)

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Task
import Random
import Time exposing (Time)
import Process

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
  ( { counter = 0
    , running = False
    }
  , Cmd.none
  )

type alias Model = 
  { counter : Int
  , running : Bool
  }
  
view : Model -> Html (Msg Model)
view model =
  div
    []
    [ text ""
    , button [onClick (runAction <| updateModel (\m -> { m | counter = m.counter + 1 }))] [text "inc"]
    , button [onClick (runAction <| updateModel (\m -> { m | counter = m.counter - 1 }))] [text "dec"]
    , button [onClick <| runAction startstop] [text (if model.running then "stop" else "start")]
    , text (toString model)
    ]

startstop : Action Model ()
startstop =
  getModel >>= \model ->
    if model.running then
      updateModel (\model -> { model | running = False }) 
    else 
      updateModel (\model -> { model | running = True }) >>= \_ ->
      runLoop

runLoop : Action Model ()
runLoop =
  getModel >>= \model ->
    if not model.running then 
      pureAction ()
    else 
      liftCommand (randomInt (0,10)) >>= \r ->
      updateModel (\m -> { m | counter = m.counter + r }) >>= \_ ->
      liftTask (Process.sleep (0.5 * Time.second)) >>= \_ ->
      runLoop

-------------------------------------------------------------------------------

randomInt : (Int, Int) -> (Int -> a) -> Cmd a
randomInt (min, max) f = Random.generate f (Random.int min max)

-------------------------------------------------------------------------------




