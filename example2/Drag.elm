module Drag exposing(..)

import Dict exposing (Dict)
import Json.Decode exposing (Value, (:=))
import Json.Decode as Json
import Monocle.Lens as Lens
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional
import Monocle.Optional exposing (Optional)
import Mouse

import Gum.Msg exposing (..)
import Gum.Action exposing (..)
import Gum.Lens exposing (..)


type alias Drag a =
  { start   : Mouse.Position
  , current : Mouse.Position
  , value   : a
  }

getDeltaPosition : Drag a -> (Int, Int)
getDeltaPosition {start,current} = (current.x - start.x, current.y - start.y)

moveByDeltaPosition : Maybe (Drag a) -> (Int, Int) -> (Int, Int)
moveByDeltaPosition mbdrag =
  case mbdrag of
    Nothing ->
      identity
    Just drag ->
      let 
        (dx, dy) = getDeltaPosition drag
      in 
        \(x, y) -> (x + dx, y + dy)

moveByDeltaPositionWhen : Maybe (Drag comparable) -> (comparable, (Int, Int)) -> (comparable, (Int, Int))
moveByDeltaPositionWhen mbdrag (key, (x, y)) =
  case mbdrag of
    Nothing ->
      (key, (x, y))
    Just drag ->
      if drag.value == key then 
        let 
          (dx, dy) = getDeltaPosition drag
        in 
          (key, (x + dx, y + dy))
      else
        (key, (x, y))

startDrag : Lens model (Maybe (Drag a)) -> a -> Mouse.Position -> Msg model
startDrag drag a pos = runAction <| setModelAt drag (Just (Drag pos pos a))


dragSubscription : Lens model (Maybe (Drag a)) -> (a -> Lens model (Int, Int)) -> model -> Sub (Msg model)
dragSubscription drag point model =
  case drag.get model of
      Nothing -> 
        Sub.none
      Just d  -> 
        Sub.batch 
          [ Mouse.moves 
            (\pos -> runAction <|
              updateModelAt drag <| 
                Maybe.map (\drag -> { drag | current = pos })
            )
        , Mouse.ups 
          (\pos -> runAction <|
            setModelAt drag Nothing >>= \_ ->
            updateModelAt (point d.value) (moveByDeltaPosition (Just d)) 
          )
        ]

dragSubscriptionAction : Lens model (Maybe (Drag a)) -> (a -> (Int, Int) -> Action model ()) -> model -> Sub (Msg model)
dragSubscriptionAction drag cont model =
  case drag.get model of
      Nothing -> 
        Sub.none
      Just d  -> 
        Sub.batch 
          [ Mouse.moves 
            (\pos -> runAction <|
              updateModelAt drag <| 
                Maybe.map (\drag -> { drag | current = pos })
            )
        , Mouse.ups 
          (\pos -> runAction <|
            setModelAt drag Nothing >>= \_ -> 
            cont d.value (getDeltaPosition d)
          )
        ]
