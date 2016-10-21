module Gum exposing (..)

import Task exposing (Task)

type Msg model 
  = Peek (model -> Msg model)
  | Poke (model -> model) (Msg model)
  | Call (Cmd (Msg model))
  | End

update : Msg model -> model -> (model, Cmd (Msg model))
update msg model =
  case msg of
    End ->
      ( model 
      , Cmd.none 
      )
    
    Peek f ->
      update (f model) model
    
    Poke upd msg' ->
      update msg' (upd model)
    
    Call cmd ->
      ( model
      , cmd
      )

task : Task e a -> (Result e a -> Msg model) -> Cmd (Msg model)
task t f = Task.perform (f << Err) (f << Ok) t
