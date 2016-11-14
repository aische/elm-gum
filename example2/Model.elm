module Model exposing(..)

import Dict exposing (Dict)
import Json.Decode exposing (Value, (:=))
import Json.Decode as Json
import Monocle.Lens as Lens
import Monocle.Lens exposing (Lens)
import Mouse

import Gum.Msg exposing (..)
import Gum.Action exposing (..)

import Ajax exposing (..)
import Drag exposing (..)


type alias Model = 
  { counter : Int
  , inputText : String
  , position : Maybe Mouse.Position
  , track : Bool
  , item : Maybe Item
  , randomNumbers : NumberLists
  , drag : Maybe (Drag ())
  , circle : (Int, Int)
  , drag2 : Maybe (Drag Int)
  , circles : Dict Int (Int, Int)
  }

type alias NumberLists = Dict String (List Int)

addToNumberLists : String -> Int -> NumberLists -> NumberLists
addToNumberLists name value = Dict.update name (\mbvs -> Just (value :: Maybe.withDefault [] mbvs) )
-------------------------------------------------------------------------------
counter : Lens Model Int
counter = { get = .counter, set = \x m -> { m | counter = x }}

position : Lens Model (Maybe Mouse.Position)
position = { get = .position, set = \x m -> { m | position = x }}

track : Lens Model Bool
track = { get = .track, set = \x m -> { m | track = x }}

item : Lens Model (Maybe Item)
item = { get = .item, set = \x m -> { m | item = x }}

inputText : Lens Model String
inputText = { get = .inputText, set = \x m -> { m | inputText = x }}

randomNumbers : Lens Model NumberLists
randomNumbers = { get = .randomNumbers, set = \x m -> { m | randomNumbers = x }}

drag : Lens Model (Maybe (Drag ()))
drag = { get = .drag, set = \x m -> { m | drag = x }}

circle : Lens Model (Int, Int)
circle = { get = .circle, set = \x m -> { m | circle = x }}

drag2 : Lens Model (Maybe (Drag Int))
drag2 = { get = .drag2, set = \x m -> { m | drag2 = x }}

circles : Lens Model (Dict Int (Int, Int))
circles = { get = .circles, set = \x m -> { m | circles = x }}

-------------------------------------------------------------------------------

circleAt : Int -> Lens Model (Maybe (Int, Int))
circleAt key = 
  { get = \model -> Dict.get key model.circles
  , set = \pos model -> {model | circles = Dict.update key (always pos) model.circles }
  }





