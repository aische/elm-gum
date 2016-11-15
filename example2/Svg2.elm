module Svg2 exposing(..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onCheck, onWithOptions, defaultOptions)
import Http
import Json.Decode exposing (Value)
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


view : Lens model (Maybe (Drag Int)) -> Lens model (Dict Int (Int, Int)) -> model -> Html (Msg model)
view drag circles model =  
  let 
    draggedId =
      Maybe.map .value (drag.get model)
  in
    Svg.svg
      [ SA.width "500"
      , SA.height "400"
      , A.style [("background-color", "#eeeeee")]
      ]
      (List.map 
        (\(key, (x, y)) -> 
          Svg.circle
            [ SA.cx (toString x)
            , SA.cy (toString y)
            , SA.r "50"
            , SA.fill (if draggedId == Just key then "red" else "black")
            , onWithOptions "mousedown" 
                {defaultOptions | stopPropagation = True, preventDefault = True } 
                (Json.map (startDrag drag key) Mouse.position)
            ]
            []
        )
        (List.map (moveByDeltaPositionWhen (drag.get model)) <| Dict.toList (circles.get model))
      )

subscriptions : Lens model (Maybe (Drag Int)) -> Lens model (Dict Int (Int, Int)) -> model -> Sub (Msg model)
subscriptions drag circles model =
  dragSubscriptionAction drag 
    (\key (dx, dy) -> 
      updateModel (Lens.modify circles (Dict.update key 
                (Maybe.map (\(x,y) -> (x+dx, y+dy))))) 
    )
    model 

