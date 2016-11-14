module Main exposing(..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Value, (:=))
import Json.Decode as Json
import Monocle.Common exposing ((=>), maybe)
import Monocle.Lens as Lens
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional
import Monocle.Optional exposing (Optional)
import Mouse
import Process
import Random
import String
import Task

import Gum.Msg exposing (..)
import Gum.Action exposing (..)
import Gum.Lens exposing (..)
import View exposing (..)
import Model exposing (..)
import Lib exposing (..)

import Counter 
import MouseTracker
import Drag exposing (..)
import Svg1 
import Svg2


main = App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : (Model, Cmd (Msg Model))
init = 
  ( initialModel
  , Cmd.none
  )

initialModel : Model 
initialModel = 
  { counter = 0
  , inputText = ""
  , position = Nothing
  , track = False
  , item = Nothing
  , randomNumbers = Dict.empty
  , drag = Nothing
  , circle = (100, 100)
  , drag2 = Nothing
  , circles = Dict.fromList [(0, (100, 100)), (1, (200, 200))]
  }

subscriptions : Model -> Sub (Msg Model)
subscriptions model = 
  Sub.batch 
  [ MouseTracker.subscriptions track position model
  , Svg1.subscriptions drag circle model
  , Svg2.subscriptions drag2 circles model
  ]



