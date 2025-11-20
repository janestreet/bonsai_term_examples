(* $MDX part-begin=hello-world *)
open! Core
open! Bonsai_term
open Bonsai.Let_syntax
module Typo = Bonsai_tui_typography
module Border_box = Bonsai_tui_border_box

let text =
  let red = Attr.Color.rgb ~r:255 ~g:0 ~b:0 in
  let green = Attr.Color.rgb ~r:0 ~g:255 ~b:0 in
  [ [ Typo.Text.of_string ~attr:[ Attr.bg red ] "hello "
    ; Typo.Text.of_string ~attr:[ Attr.bg green ] "world"
    ]
  ; [ Typo.Text.of_string ~attr:[] "" ]
  ; [ Typo.Text.of_string ~attr:[ Attr.bg red ] "hello-"
    ; Typo.Text.of_string ~attr:[ Attr.bg green ] "world"
    ]
  ; [ Typo.Text.of_string ~attr:[] "" ]
  ; [ Typo.Text.of_string ~attr:[ Attr.bg red ] "this-is-a-very-long-word-" ]
  ]
;;

let typeset_text ~max_width =
  Typo.typeset ~max_width text
  |> List.map ~f:(fun line ->
    List.map line ~f:(fun ({ Typo.Text.attr = attrs; _ } as text) ->
      View.text ~attrs (Typo.Text.to_string text))
    |> View.hcat)
  |> View.vcat
;;

let animation_cycle graph =
  let%arr now = Bonsai.Clock.Expert.now graph in
  Int.of_float
    ((Float.sin (Time_ns.Span.to_sec_approx (Time_ns.diff now Time_ns.epoch)) +. 1.0)
     /. 2.0
     *. 20.0)
;;

let app ~dimensions:_ (local_ graph) =
  let view =
    let%arr max_width = animation_cycle graph in
    let text = typeset_text ~max_width in
    let box =
      Border_box.view
        (View.transparent_rectangle ~width:max_width ~height:(View.height text))
    in
    View.zcat [ View.pad ~t:1 ~l:1 text; box ]
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
