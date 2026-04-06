open! Core
open! Bonsai_term

let command =
  let open Async in
  Command.async_or_error ~summary:{|Bonsai term text editor demo!|}
  @@
  let%map_open.Command () = return () in
  fun () ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () =
      Bonsai_term.start (fun ~dimensions (graph @ local) ->
        let open Bonsai.Let_syntax in
        let ( ~view
            , ~handler
            , ~toggle_keybindings_mode:_
            , ~text:_
            , ~set_text:_
            , ~mode:_
            , ~get_cursor_position:_ )
          =
          Bonsai_term_text_editor_example.app ~dimensions graph
        in
        (* Wrap the handler to drop the Captured_or_ignored result *)
        let handler =
          let%arr handler in
          fun event ->
            let%bind.Effect (_ : Captured_or_ignored.t) = handler event in
            Effect.return ()
        in
        ~view, ~handler)
    in
    return ()
;;

let () = Command_unix.run command
