module Gum.Lens exposing (..)

import Gum.Action exposing (..)

import Monocle.Common exposing ((=>), maybe)
import Monocle.Lens as Lens
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional
import Monocle.Optional exposing (Optional)


setModelAt : Lens model a -> a -> Action model ()
setModelAt lens a = updateModel (lens.set a)

getModelAt : Lens model a -> Action model a
getModelAt lens = getModel >>= \model -> pureAction (lens.get model)

updateModelAt : Lens model a -> (a -> a) -> Action model ()
updateModelAt lens f = updateModel (Lens.modify lens f)

setModelAtOpt : Optional model a -> a -> Action model ()
setModelAtOpt lens a = updateModel (lens.set a)

getModelAtOpt : Optional model a -> Action model (Maybe a)
getModelAtOpt lens = getModel >>= \model -> pureAction (lens.getOption model)

updateModelAtOpt : Optional model a -> (a -> a) -> Action model ()
updateModelAtOpt lens f = updateModel (Optional.modify lens f)

