module Counter exposing(..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
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
import Task

import Gum.Msg exposing (..)
import Gum.Action exposing (..)

view : Lens model Int -> model -> Html (Msg model)
view counter model =
  div []
    [ button 
        [ onClick (runAction <| dec counter)
        ] 
        [ text "--"
        ]
    , button 
        [ onClick (runAction <| inc counter)
        ] 
        [ text "++"
        ]
    , text (toString (counter.get model))
    ]


inc : Lens m Int -> Action m ()
inc counter = updateModel (Lens.modify counter (\x -> x + 1))

dec : Lens m Int -> Action m ()
dec counter = updateModel (Lens.modify counter (\x -> x - 1))

