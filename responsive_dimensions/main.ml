open! Core
open! Bonsai_term
open Bonsai.Let_syntax

let app ~dimensions (local_ _graph) =
  let view =
    let%arr { Dimensions.width; height } = dimensions in
    let number_attr =
      Attr.many [ Attr.fg (Bonsai_tui_catpuccin.color ~flavor:Mocha Blue); Attr.bold ]
    in
    let number n = View.text ~attrs:[ number_attr ] [%string "%{n#Int}"] in
    let image =
      View.hcat
        [ View.text "("; number width; View.text " x "; number height; View.text ")" ]
    in
    View.center image ~within:{ width; height }
  in
  let handler = Bonsai.return (fun _ -> Effect.Ignore) in
  ~view, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:{|Capy dimensions!|}
    [%map_open.Command
      let () = return () in
      fun () -> Bonsai_term.start app]
;;

let () = Command_unix.run command
