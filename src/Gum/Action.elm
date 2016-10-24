module Gum.Action exposing (..)

import Task exposing (Task)
import Gum exposing (..)

type Action model a = Action ((a -> Msg model) -> Msg model)

unAction : Action model a -> ((a -> Msg model) -> Msg model)
unAction (Action a) = a

runAction : Action model () -> Msg model
runAction (Action f) = f (always End)

liftCommand : ((a -> Msg model) -> Cmd (Msg model)) -> Action model a
liftCommand cmd = Action <| \cont ->
  Call <| cmd cont 

andThen : Action model a -> (a -> Action model b) -> Action model b
andThen (Action ma) f = 
  Action <| \cont ->
    ma <| \a ->
      unAction (f a) cont

returnAction : a -> Action model a
returnAction a = Action <| \cont -> cont a

getModel : Action model model 
getModel =
  Action <| \cont ->
    Peek <| \model ->
      cont model

setModel : model -> Action model ()
setModel model = 
  Action <| \cont ->
    Poke (always model) (cont ())

updateModel : (model -> model) -> Action model ()
updateModel f = 
  Action <| \cont ->
    Poke f (cont ())

liftTask : Task e a -> Action model (Result e a)
liftTask t =
  Action <| \cont -> 
    Call <| task t cont  

runCmd : Action model () -> Cmd (Msg model)
runCmd (Action f) = 
  Task.perform (always End) f (Task.succeed (always End))

