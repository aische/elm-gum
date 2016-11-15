# elm-gum

An Elm library with Generic Update and Message and a monad

## Usage

With elm-gum, you don't have to write your own message type and update function, you simply use the Msg type from elm-gum and its update function.

There are five different messages than can be composed:

```elm

type Msg model
    = Peek (model -> Msg model)
    | Poke (model -> model) (Msg model)
    | Call (Cmd (Msg model))
    | Fork (Msg model) (Msg model)
    | End

```

Now you can write imperative looking code directly in your views!

Note: This is an experiment and I don't think it fits well into the philosophy of Elm.

With the Msg of elm-gum a monadic Action type can be constructed:

```elm

type Action model a = Action ((a -> Msg model) -> Msg model)

```

## Example for using the Msg type

```elm

module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Task
import Random
import Time exposing (Time)
import Process
import Gum.Msg exposing (..)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub (Msg Model)
subscriptions model =
    Sub.none


init : ( Model, Cmd (Msg Model) )
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
        , button [ onClick (Poke (\m -> { m | counter = m.counter + 1 }) End) ] [ text "inc" ]
        , button [ onClick (Poke (\m -> { m | counter = m.counter - 1 }) End) ] [ text "dec" ]
        , button [ onClick startstop ]
            [ text
                (if model.running then
                    "stop"
                 else
                    "start"
                )
            ]
        , text (toString model)
        ]


startstop : Msg Model
startstop =
    Peek <|
        \model ->
            if model.running then
                Poke (\model -> { model | running = False })
                    End
            else
                Poke (\model -> { model | running = True })
                    runLoop


runLoop : Msg Model
runLoop =
    Peek <|
        \model ->
            if not model.running then
                End
            else
                Call <|
                    randomInt ( 0, 10 ) <|
                        \r ->
                            Poke (\m -> { m | counter = m.counter + r }) <|
                                Call <|
                                    sleep (0.5 * Time.second)
                                        runLoop


randomInt : ( Int, Int ) -> (Int -> a) -> Cmd a
randomInt ( min, max ) f =
    Random.generate f (Random.int min max)


sleep : Time -> a -> Cmd a
sleep t a =
    Task.perform (\x -> a) (\x -> a) (Process.sleep t)



```

## Example for using the Action monad

Same program as above using the Action monad:

```elm

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

randomInt : (Int, Int) -> (Int -> a) -> Cmd a
randomInt (min, max) f = Random.generate f (Random.int min max)

```
