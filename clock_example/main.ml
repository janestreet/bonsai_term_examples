open! Core
open Bonsai_term
open Bonsai.Let_syntax

let ( >> ) a b = Fn.compose a b

let render_date (date : Date.t) =
  let t f =
    View.text
      ~attrs:[ Attr.fg (Bonsai_tui_catpuccin.color ~flavor:Mocha Blue) ]
      (Int.to_string (f date))
  in
  let year = t Date.year in
  let month = t (Month.to_int >> Date.month) in
  let day = t Date.day in
  View.hcat [ year; View.text "-"; month; View.text "-"; day ]
;;

let render_time_of_day (time_of_day : Time_ns.Ofday.t) =
  View.text
    ~attrs:[ Attr.bold; Attr.fg (Bonsai_tui_catpuccin.color ~flavor:Mocha Green) ]
    (Time_ns.Ofday.to_sec_string time_of_day)
;;

let clock_app ~dimensions (local_ graph) =
  let view =
    let%arr { Dimensions.width; height } = dimensions
    and now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) graph in
    let date = Time_ns.to_date ~zone:(force Timezone.local) now in
    let date = render_date date in
    let time_of_day = Time_ns.to_ofday ~zone:(force Timezone.local) now in
    let time_of_day = render_time_of_day time_of_day in
    View.center ~within:{ width; height } (View.hcat [ date; View.text " "; time_of_day ])
  in
  let handler = Bonsai.return (fun _ -> Effect.Ignore) in
  ~view, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:{|A bonsai_term demo showcasing clock!|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start clock_app)
;;

let () = Command_unix.run command
