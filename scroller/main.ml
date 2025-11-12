open! Core
open Async
open Bonsai_term
open Bonsai.Let_syntax

let app ~dimensions (local_ graph) =
  let mli = [%embed_file_as_string "../../tui/scroller/src/bonsai_tui_scroller.mli"] in
  let text =
    {%string|
(** This is a demo of [bonsai_tui_scroller], a library that allows you to create a
    "scrollable" region.

    You can scroll around this demo using less keybindings. 

    Here is the MLI for [bonsai_tui_scroller.mli]:
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
  let%sub ~view, ~less_keybindings_handler, .. =
    Bonsai_tui_scroller.component ~dimensions view graph
  in
  ~view, ~handler:less_keybindings_handler
;;

let command =
  let open Deferred.Or_error.Let_syntax in
  Command.async_or_error ~summary:{|Demo of bonsai_tui_scroller.|}
  @@
  let%map_open.Command () = return () in
  fun () ->
    let%bind () = Bonsai_term.start app in
    return ()
;;

let () = Command_unix.run command
