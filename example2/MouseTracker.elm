module MouseTracker exposing(..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onCheck)
import Http
import Json.Decode exposing (Value)
import Json.Decode as Json
import Monocle.Common exposing ((=>), maybe)
import Monocle.Lens as Lens
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional
import Monocle.Optional exposing (Optional)
import Mouse exposing (Position)
import Process
import Random
import String
import Task

import Gum.Msg exposing (..)
import Gum.Action exposing (..)


view : Lens m Bool -> Lens m (Maybe Position) -> m -> Html (Msg m)
view track position model =
  div []
    [ Html.input 
        [ A.type_ "checkbox"
        , A.checked (track.get model)
        , onCheck (runAction << setTracking track position)
        ]
        []
    , text <| Maybe.withDefault "" <| Maybe.map toString (position.get model)
    ]

setTracking : Lens m Bool -> Lens m (Maybe Mouse.Position) -> Bool -> Action m ()
setTracking track position b =
  if b then
    updateModel (track.set b)
  else 
    updateModel (track.set b << position.set Nothing)

subscriptions : Lens m Bool -> Lens m (Maybe Mouse.Position) -> m -> Sub (Msg m)
subscriptions track position model =
  if track.get model then
    Mouse.moves (\p -> runAction (updateModel (position.set (Just p))))
  else
    Sub.none
