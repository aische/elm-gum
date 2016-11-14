module Lib exposing (..)

import Http
import Json.Decode exposing (Value, (:=))
import Json.Decode as Json

import Monocle.Common exposing ((=>), maybe)
import Monocle.Lens as Lens
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional
import Monocle.Optional exposing (Optional)

import Process
import Random

import Gum.Msg exposing (..)
import Gum.Action exposing (..)

-------------------------------------------------------------------------------
load : Json.Decoder a -> String -> Action model (Result Http.Error a)
load decoder url =
  liftTask (Http.get decoder url)

sleep : Float -> Action model ()
sleep t = 
  mapAction (Result.withDefault ()) <|
  liftTask (Process.sleep t)

random : (Int, Int) -> Action model Int
random (min, max) = 
  liftCommand <| \cont -> Random.generate cont (Random.int min max)

-------------------------------------------------------------------------------


