module Svg1 exposing(..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onCheck, onWithOptions, defaultOptions)
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
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task

import Gum.Msg exposing (..)
import Gum.Action exposing (..)
import Gum.Lens exposing (..)

import Drag exposing (..)


view : Lens model (Maybe (Drag ())) -> Lens model (Int, Int) -> model -> Svg (Msg model)
view drag circle model = 
  let
    (x, y) = moveByDeltaPosition (drag.get model) (circle.get model)
  in
    Svg.svg
      [ SA.width "500"
      , SA.height "400"
      ]
      [ Svg.circle
          [ SA.cx (toString x)
          , SA.cy (toString y)
          , SA.r "50"
          , onWithOptions "mousedown" 
              {defaultOptions | stopPropagation = True, preventDefault = True } 
              (Json.map (startDrag drag ()) Mouse.position)
          ]
          []
      ]

subscriptions : Lens model (Maybe (Drag ())) -> Lens model (Int, Int) -> model -> Sub (Msg model)
subscriptions drag circle model = 
  dragSubscription drag (\_ -> circle) model

