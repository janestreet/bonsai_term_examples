(* $MDX part-begin=hello-world *)
open! Core
open! Bonsai_term
open Bonsai.Let_syntax

let app ~(dimensions : Dimensions.t Bonsai.t) (local_ _graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let view =
    let%arr dimensions in
    let custom_fg = Attr.Color.rgb ~r:255 ~g:110 ~b:100 in
    let custom_bg = Attr.Color.rgb ~r:55 ~g:10 ~b:0 in
    View.vcat
      [ View.text "no fg or bg"
      ; View.text ~attrs:[ Attr.fg custom_fg ] "only fg styled"
      ; View.text ~attrs:[ Attr.bg custom_bg ] "only bg styled"
      ; View.text ~attrs:[ Attr.fg custom_fg; Attr.bg custom_bg ] "both fg and bg styled"
      ]
    |> View.center ~within:dimensions
    |> View.with_colors
         ~fill_backdrop:true
         ~fg:(Attr.Color.rgb ~r:100 ~g:110 ~b:255)
         ~bg:(Attr.Color.rgb ~r:0 ~g:10 ~b:55)
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
