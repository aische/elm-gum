module Gum.Action exposing (..)

import Gum.Msg exposing (..)
import Task exposing (Task)


type Action model a = 
  Action ((a -> Msg model) -> Msg model)

unAction : Action model a -> ((a -> Msg model) -> Msg model)
unAction (Action a) = 
  a

runAction : Action model () -> Msg model
runAction (Action f) = 
  f (always End)

pureAction : a -> Action model a
pureAction a = 
  Action <| \cont -> 
    cont a

mapAction : (a -> b) -> Action model a -> Action model b
mapAction f (Action g) = 
  Action <| \cont ->
    g (\a -> cont (f a))

apAction : Action model (a -> b) -> Action model a -> Action model b
apAction (Action ff) (Action ga) = 
  Action <| \cont ->
    ff (\f -> ga (\a -> cont (f a)))

andThen : Action model a -> (a -> Action model b) -> Action model b
andThen (Action ma) f = 
  Action <| \cont ->
    ma <| \a ->
      unAction (f a) cont

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

forkAction : Action model () -> Action model ()
forkAction (Action a) =
  Action <| \cont ->
    Fork (a (always End)) (cont ())

liftCommand : ((a -> Msg model) -> Cmd (Msg model)) -> Action model a
liftCommand cmd = 
  Action <| \cont ->
    Call (cmd cont)

liftTask : Task e a -> Action model (Result e a)
liftTask t =
  Action <| \cont -> 
    Call (task t cont)

runCmd : Action model () -> Cmd (Msg model)
runCmd (Action f) = 
  Task.perform (always End) f (Task.succeed (always End))
  --Task.perform (always End) identity (Task.succeed (f (always End)))

(<*>) : (a -> b) -> Action model a -> Action model b
(<*>) = mapAction

(<$>) : Action model (a -> b) -> Action model a -> Action model b
(<$>) = apAction

(>>=) : Action model a -> (a -> Action model b) -> Action model b
(>>=) = andThen

forkActions : List (Action model ()) -> Action model ()
forkActions actions =
  case actions of
    [] ->
      pureAction ()
    a::r ->
      forkAction a >>= \_ -> forkActions r

onErrorAction : (e -> Action model ()) -> Result e a -> Action model a
onErrorAction f result = Action <| \cont ->
  case result of
    Err e ->
      unAction (f e) (always End)
    Ok a ->
      cont a

