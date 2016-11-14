module View exposing(..)

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

import Lib exposing (..)
import Model exposing (..)
import Counter 
import Ajax exposing (..)
import MouseTracker 
import Drag exposing (..)
import Svg1 
import Svg2 

view : Model -> Html (Msg Model)
view model =
  div []
  [
    vlist
      [ Counter.view counter model
      , button 
          [ onClick (runAction <| loadItem item)
          ] 
          [ text "ajax"
          ]
      , button 
          [ onClick (runAction <| setModelAt item Nothing)
          ] 
          [ text "reset ajax"
          ]
      , Html.input 
          [ A.value model.inputText
          , onInput (runAction << setModelAt inputText)
          ]
          []
      , Html.input 
          [ A.value model.inputText
          , onInput (runAction << setModelAt inputText)
          ]
          []
      , MouseTracker.view track position model
      , button 
          [ onClick <| runAction <| forkActions
            [ addRandomNumber randomNumbers "a"
            , addRandomNumber randomNumbers "b"
            , addRandomNumber randomNumbers "c"
            , addRandomNumber randomNumbers "d"
            , addRandomNumber randomNumbers "e"
            ]
          ] 
          [ text "rand"
          ]
      , button 
          [ onClick <| runAction <| forkActions
            [ addRandomNumber randomNumbers "a"
            , addRandomNumber randomNumbers "b"
            , addRandomNumber randomNumbers "f"
            , addRandomNumber randomNumbers "g"
            , addRandomNumber randomNumbers "h"
            ]
          ] 
          [ text "rand"
          ]
      , button 
          [ onClick <| runAction <| 
            --load decodeItem "elm-package.json" >>=
            load decodeItem "Lib.elm" >>=
            onErrorAction (setModelAt inputText << toString) >>= \a ->
            setModelAt item (Just a)
          ] 
          [ text "ajax that fails"
          ]
      ]
  , vlist
    [ Svg1.view drag circle model
    , Svg2.view drag2 circles model

    , text (toString model)
    ]
  ]

vlist elems =
  div [ A.style [("display", "flex"), ("flex-flow", "row wrap")]]
  (
    List.map 
      (\e ->
        div [] [e]
      )
    elems
  )


addRandomNumber : Lens m NumberLists -> String -> Action m ()
addRandomNumber dict name =
  updateModelAt dict (Dict.remove name) >>= \_ ->
  random (0, 1000) >>= \r1 ->
  sleep (toFloat r1) >>= \_ -> 
  updateModelAt dict (addToNumberLists name r1) >>= \_ -> 
  random (0, 1000) >>= \r2 ->
  sleep (2000 + toFloat r2) >>= \_ -> 
  updateModelAt dict (addToNumberLists name r2)

