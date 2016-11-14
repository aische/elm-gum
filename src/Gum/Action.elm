module Gum.Action exposing (..)

import Gum.Msg exposing (..)
import Task exposing (Task)


{-| Action monad parametrized over model and result type
-}
type Action model a = 
  Action ((a -> Msg model) -> Msg model)

{-| Deconstructor
-}
unAction : Action model a -> ((a -> Msg model) -> Msg model)
unAction (Action a) = 
  a

{-| Turn Action into Msg
-}
runAction : Action model () -> Msg model
runAction (Action f) = 
  f (always End)

{-| Create pure Action
-}
pureAction : a -> Action model a
pureAction a = 
  Action <| \cont -> 
    cont a

{-| mapAction (Functor)
-}
mapAction : (a -> b) -> Action model a -> Action model b
mapAction f (Action g) = 
  Action <| \cont ->
    g (\a -> cont (f a))

{-| apAction (Applicative)
-}
apAction : Action model (a -> b) -> Action model a -> Action model b
apAction (Action ff) (Action ga) = 
  Action <| \cont ->
    ff (\f -> ga (\a -> cont (f a)))

{-| andThen (Monad)
-}
andThen : Action model a -> (a -> Action model b) -> Action model b
andThen (Action ma) f = 
  Action <| \cont ->
    ma <| \a ->
      unAction (f a) cont

{-| Get current state of model
-}
getModel : Action model model 
getModel =
  Action <| \cont ->
    Peek <| \model ->
      cont model

{-| Set state of model
-}
setModel : model -> Action model ()
setModel model = 
  Action <| \cont ->
    Poke (always model) (cont ())

{-| Update current state of model
-}
updateModel : (model -> model) -> Action model ()
updateModel f = 
  Action <| \cont ->
    Poke f (cont ())

{-| Fork an Action, then run another Action. The first Action is performed until the first Command appears, then the second is started.
-}
forkAction : Action model () -> Action model ()
forkAction (Action a) =
  Action <| \cont ->
    Fork (a (always End)) (cont ())

{-| Run a Command in the Action monad
-}
liftCommand : ((a -> Msg model) -> Cmd (Msg model)) -> Action model a
liftCommand cmd = 
  Action <| \cont ->
    Call (cmd cont)

{-| Lift a Task into the Action monad
-}
liftTask : Task e a -> Action model (Result e a)
liftTask t =
  Action <| \cont -> 
    Call (task t cont)

{-| Turn an Action into a Command
-}
runCmd : Action model () -> Cmd (Msg model)
runCmd (Action f) = 
  Task.perform (always End) f (Task.succeed (always End))
  --Task.perform (always End) identity (Task.succeed (f (always End)))

{-| Shortcut for mapAction
-}
(<*>) : (a -> b) -> Action model a -> Action model b
(<*>) = mapAction

{-| Shortcut for apAction
-}
(<$>) : Action model (a -> b) -> Action model a -> Action model b
(<$>) = apAction

{-| Shortcut for andThen
-}
(>>=) : Action model a -> (a -> Action model b) -> Action model b
(>>=) = andThen

{-| Run many Actions in parallel. Parallelism starts as soon as there are Commands, otherwise this will run the Actions in sequence.
-}
forkActions : List (Action model ()) -> Action model ()
forkActions actions =
  case actions of
    [] ->
      pureAction ()
    a::r ->
      forkAction a >>= \_ -> forkActions r

{-| Shortcut for dealing with Result.Err results of Actions
-}
onErrorAction : (e -> Action model ()) -> Result e a -> Action model a
onErrorAction f result = Action <| \cont ->
  case result of
    Err e ->
      unAction (f e) (always End)
    Ok a ->
      cont a

