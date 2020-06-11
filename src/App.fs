module App

open Elmish

module Counter =
    type Model = { count: int }

    let init () = { count = 0 }, Cmd.none

    type Msg =
        | Increment
        | Decrement

    let update msg model =
        match msg with
        | Increment -> { model with count = model.count + 1 }, Cmd.none
        | Decrement -> { model with count = model.count - 1 }, Cmd.none

type Model =
    { top: Counter.Model
      bottom: Counter.Model }

type Msg =
    | Reset
    | Top of Counter.Msg
    | Bottom of Counter.Msg

let init () =
    let top, topCmd = Counter.init ()
    let bottom, bottomCmd = Counter.init ()
    { top = top; bottom = bottom },
    Cmd.batch
        [ Cmd.map Top topCmd
          Cmd.map Bottom bottomCmd ]

let update msg model: Model * Cmd<Msg> =
    match msg with
    | Reset ->
        let top, topCmd = Counter.init ()
        let bottom, bottomCmd = Counter.init ()
        { top = top; bottom = bottom },
        Cmd.batch
            [ Cmd.map Top topCmd
              Cmd.map Bottom bottomCmd ]
    | Top msg' ->
        let res, cmd = Counter.update msg' model.top
        { model with top = res }, Cmd.map Top cmd
    | Bottom msg' ->
        let res, cmd = Counter.update msg' model.bottom
        { model with bottom = res }, Cmd.map Bottom cmd

let view model _ = printf "%A\n" model

Program.mkProgram init update view |> Program.run
