open! Core

let command =
  let open Async in
  Command.async_or_error ~summary:{|Bonsai term text editor demo!|}
  @@
  let%map_open.Command () = return () in
  fun () ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      Bonsai_term.start (fun ~dimensions (local_ graph) ->
        let ( ~view
            , ~handler
            , ~toggle_keybindings_mode:_
            , ~text:_
            , ~set_text:_
            , ~get_cursor_position:_ )
          =
          Bonsai_tui_text_editor_example.app ~dimensions graph
        in
        ~view, ~handler)
    in
    return ()
;;

let () = Command_unix.run command
