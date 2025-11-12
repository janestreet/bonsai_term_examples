open! Core
open! Async
open! Bonsai_term

let command =
  Command.async_or_error
    ~summary:"A tiny pomodoro timer TUI application"
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start Pomodoro_timer.app)
;;

let () = Command_unix.run command
