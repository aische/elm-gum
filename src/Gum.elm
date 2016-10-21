module Gum exposing (..)

{-|
# Generic Message
@docs Msg
# Generic update
@docs update
# Task helper
@docs task
-}

import Task exposing (Task)


{-| A generic message type parametrized with the model type
-}
type Msg model
    = Peek (model -> Msg model)
    | Poke (model -> model) (Msg model)
    | Call (Cmd (Msg model))
    | End


{-| Use this update function as the main update function
-}
update : Msg model -> model -> ( model, Cmd (Msg model) )
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


{-| Similar to Task.perform, but uses only one message expecting a (Result e a)
-}
task : Task e a -> (Result e a -> Msg model) -> Cmd (Msg model)
task t f =
    Task.perform (f << Err) (f << Ok) t
