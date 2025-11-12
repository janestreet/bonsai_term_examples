(* $MDX part-begin=hello-world *)
open! Core
open! Bonsai_term
open Bonsai.Let_syntax

let app ~(dimensions : Dimensions.t Bonsai.t) (local_ _graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let view =
    let%arr dimensions in
    View.center ~within:dimensions (View.text "Hello world!")
  in
  let handler = Bonsai.return (fun _ -> Effect.Ignore) in
  ~view, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:{|Hello world!|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;

let () = Command_unix.run command
(* $MDX part-end *)
