open! Core
open Bonsai_term
open Bonsai.Let_syntax

(** This is a demo of [bonsai_term_scroller], a library that allows you to create a
    "scrollable" region.

    You can scroll around this demo using less keybindings.

    You can run this on this file by running:

    {v
./lib/bonsai_term_examples/scroller/bin/main.exe ./lib/bonsai_term_examples/scroller/src/bonsai_term_scroller_example.ml
    v} *)

let app ~contents ~dimensions (local_ graph) =
  let view =
    Bonsai.return
    @@ View.vcat
    @@ (contents
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
  let open Async in
  Command.async_or_error ~summary:{|Demo of bonsai_term_scroller.|}
  @@
  let%map_open.Command () = return ()
  and file = anon ("FILE" %: File_path.arg_type) in
  fun () ->
    let%bind contents = Filesystem_async.read_file file in
    Bonsai_term.start (fun ~dimensions graph -> app ~contents ~dimensions graph)
;;
