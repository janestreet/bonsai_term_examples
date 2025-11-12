open! Core
open! Async
open Bonsai_term
open Bonsai.Let_syntax

let work_duration = Time_ns.Span.of_min 25.
let short_break_duration = Time_ns.Span.of_min 5.
let long_break_duration = Time_ns.Span.of_min 15.

module Phase = struct
  type t =
    | Work
    | Short_break
    | Long_break

  let duration = function
    | Work -> work_duration
    | Short_break -> short_break_duration
    | Long_break -> long_break_duration
  ;;

  let name = function
    | Work -> "Work"
    | Short_break -> "Short Break"
    | Long_break -> "Long Break"
  ;;

  let next_phase ~completed_pomodoros = function
    | Work ->
      let completed = completed_pomodoros + 1 in
      if completed % 4 = 0 then Long_break, completed else Short_break, completed
    | Short_break | Long_break -> Work, completed_pomodoros
  ;;
end

module Timer_state = struct
  type t =
    | Stopped
    | Running
    | Paused
end

(* Model for the pomodoro timer *)
module Model = struct
  type t =
    { phase : Phase.t
    ; timer_state : Timer_state.t
    ; remaining_time : Time_ns.Span.t
    ; completed_pomodoros : int
    }

  let initial =
    { phase = Work
    ; timer_state = Stopped
    ; remaining_time = work_duration
    ; completed_pomodoros = 0
    }
  ;;
end

(* Actions that can be performed on the timer *)
module Action = struct
  type action =
    | Start
    | Pause
    | Reset
    | Tick
    | Next_phase
end

let apply_action _context model = function
  | Action.Start ->
    (match model.Model.timer_state with
     | Stopped | Paused -> { model with timer_state = Running }
     | Running -> model)
  | Pause ->
    (match model.timer_state with
     | Running -> { model with timer_state = Paused }
     | Stopped | Paused -> model)
  | Reset ->
    { model with timer_state = Stopped; remaining_time = Phase.duration model.phase }
  | Tick ->
    (match model.timer_state with
     | Running ->
       if Time_ns.Span.(model.remaining_time > second)
       then { model with remaining_time = Time_ns.Span.(model.remaining_time - second) }
       else (
         let new_phase, new_completed =
           Phase.next_phase ~completed_pomodoros:model.completed_pomodoros model.phase
         in
         { phase = new_phase
         ; timer_state = Stopped
         ; remaining_time = Phase.duration new_phase
         ; completed_pomodoros = new_completed
         })
     (* Timer finished, move to next phase *)
     | Stopped | Paused -> model)
  | Next_phase ->
    let new_phase, new_completed =
      Phase.next_phase ~completed_pomodoros:model.completed_pomodoros model.phase
    in
    { phase = new_phase
    ; timer_state = Stopped
    ; remaining_time = Phase.duration new_phase
    ; completed_pomodoros = new_completed
    }
;;

let format_time seconds =
  let seconds = Time_ns.Span.to_int_sec seconds in
  let minutes = seconds / 60 in
  let seconds = seconds % 60 in
  sprintf "%02d:%02d" minutes seconds
;;

let render_timer_display model (local_ graph) =
  let%arr flavor = Bonsai_tui_catpuccin.flavor graph
  and model in
  let text_color = Bonsai_tui_catpuccin.color ~flavor Text in
  let bg_color = Bonsai_tui_catpuccin.color ~flavor Crust in
  let accent_color =
    match model.Model.phase with
    | Work -> Bonsai_tui_catpuccin.color ~flavor Red
    | Short_break -> Bonsai_tui_catpuccin.color ~flavor Green
    | Long_break -> Bonsai_tui_catpuccin.color ~flavor Blue
  in
  let phase_text =
    View.text
      ~attrs:[ Attr.bold; Attr.fg accent_color; Attr.bg bg_color ]
      (Phase.name model.phase)
  in
  let time_text =
    View.text
      ~attrs:[ Attr.bold; Attr.fg text_color; Attr.bg bg_color ]
      (format_time model.remaining_time)
  in
  let state_text =
    View.text
      ~attrs:[ Attr.fg text_color; Attr.bg bg_color ]
      (match model.timer_state with
       | Stopped -> "Stopped"
       | Running -> "Running"
       | Paused -> "Paused")
  in
  let completed_text =
    View.text
      ~attrs:[ Attr.fg text_color; Attr.bg bg_color ]
      (sprintf "Completed: %d" model.completed_pomodoros)
  in
  View.vcat
    [ phase_text
    ; View.text ~attrs:[ Attr.bg bg_color ] ""
    ; time_text
    ; View.text ~attrs:[ Attr.bg bg_color ] ""
    ; state_text
    ; View.text ~attrs:[ Attr.bg bg_color ] ""
    ; completed_text
    ]
;;

let render_instructions (local_ graph) =
  let%arr flavor = Bonsai_tui_catpuccin.flavor graph in
  let text_color = Bonsai_tui_catpuccin.color ~flavor Text in
  let subtext_color = Bonsai_tui_catpuccin.color ~flavor Subtext0 in
  let bg_color = Bonsai_tui_catpuccin.color ~flavor Crust in
  let instructions =
    [ "SPACE", "Start/Pause"; "R", "Reset"; "N", "Next Phase"; "Ctrl+C", "Quit" ]
  in
  let instruction_views =
    List.map instructions ~f:(fun (key, action) ->
      View.hcat
        [ View.text ~attrs:[ Attr.bold; Attr.fg text_color; Attr.bg bg_color ] key
        ; View.text ~attrs:[ Attr.fg text_color; Attr.bg bg_color ] " - "
        ; View.text ~attrs:[ Attr.fg subtext_color; Attr.bg bg_color ] action
        ])
  in
  View.vcat
    (View.text ~attrs:[ Attr.bold; Attr.fg text_color; Attr.bg bg_color ] "Controls:"
     :: View.text ~attrs:[ Attr.bg bg_color ] ""
     :: instruction_views)
;;

let timer_state_machine (local_ graph) =
  Bonsai.state_machine ~default_model:Model.initial ~apply_action graph
;;

let keyboard_handler ~inject ~model (local_ _graph) =
  let%arr inject and model in
  fun (event : Event.t) ->
    match event with
    | Key_press { key = ASCII ' '; mods = [] } ->
      (match model.Model.timer_state with
       | Stopped -> inject Action.Start
       | Running -> inject Pause
       | Paused -> inject Start)
    | Key_press { key = ASCII 'r'; mods = [] } | Key_press { key = ASCII 'R'; mods = [] }
      -> inject Reset
    | Key_press { key = ASCII 'n'; mods = [] } | Key_press { key = ASCII 'N'; mods = [] }
      -> inject Next_phase
    | _ -> Effect.Ignore
;;

let timer_ticker ~inject (local_ graph) =
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    (Bonsai.return (Time_ns.Span.of_sec 1.0))
    (let%arr inject in
     inject Action.Tick)
    graph
;;

let backdrop ~dimensions (local_ graph) =
  let%arr { Dimensions.width; height } = dimensions
  and flavor = Bonsai_tui_catpuccin.flavor graph in
  let bg_color = Bonsai_tui_catpuccin.color ~flavor Crust in
  View.rectangle ~attrs:[ Attr.bg bg_color ] ~fill:' ' ~width ~height ()
;;

let app ~dimensions (local_ graph) =
  let model, inject = timer_state_machine graph in
  (* Set up the timer ticker *)
  let () = timer_ticker ~inject graph in
  let view =
    let%arr { Dimensions.width; height } = dimensions
    and timer_display = render_timer_display model graph
    and instructions = render_instructions graph
    and backdrop = backdrop ~dimensions graph in
    let content = View.vcat [ timer_display; View.text ""; View.text ""; instructions ] in
    let centered_content = View.center ~within:{ width; height } content in
    View.zcat [ centered_content; backdrop ]
  in
  let handler = keyboard_handler ~inject ~model graph in
  ~view, ~handler
;;
