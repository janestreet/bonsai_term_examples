open! Core
open Async
open Bonsai_term
open Bonsai.Let_syntax

let app ~dimensions (local_ graph) =
  let mli =
    [%embed_file_as_string "../../../bonsai_term/scroller/src/bonsai_term_scroller.mli"]
  in
  let text =
    {%string|
(** This is a demo of [bonsai_term_scroller], a library that allows you to create a
    "scrollable" region.

    You can scroll around this demo using less keybindings.

    Here is the MLI for [bonsai_term_scroller.mli]:
*)

%{mli}|}
  in
  let view =
    Bonsai.return
    @@ View.vcat
    @@ (String.strip text
        |> String.split_lines
        |> List.mapi ~f:(fun i line ->
          let line =
            let i = String.pad_left ~len:2 (Int.to_string (i + 1)) in
            {%string| %{i} │ %{line}|}
          in
          View.text line))
  in
  let%sub { view; less_keybindings_handler; scroll_position; _ } =
    Bonsai_term_scroller.component ~crop_width_if_too_big:`No ~dimensions view graph
  in
  let view =
    let%arr view and scroll_position and dimensions in
    let scrollbar =
      Bonsai_term_scroller.Scrollbar.Style.vertical_bar
        ~scroll_position
        ~height:dimensions.Dimensions.height
        ()
    in
    View.hcat [ view; scrollbar ]
  in
  let handler =
    let%arr less_keybindings_handler in
    fun (event : Event.t) ->
      let%bind.Effect _ : Captured_or_ignored.t = less_keybindings_handler event in
      Effect.return ()
  in
  ~view, ~handler
;;

let command =
  Command.async_or_error ~summary:{|Demo of bonsai_term_scroller.|}
  @@
  let%map_open.Command () = return () in
  fun () -> Bonsai_term.start app
;;
