module Ajax exposing(..)

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

import Lib exposing (..)


type alias Item =
  { summary : String
  , version : String
  }

decodeItem : Json.Decoder Item
decodeItem =
  Json.object2 Item 
    ("summary" := Json.string)
    ("version" := Json.string)

loadItem : Lens m (Maybe Item) -> Action m ()
loadItem item = 
  load decodeItem "elm-package.json" >>= \i -> 
  setModelAt item (Result.toMaybe i)

