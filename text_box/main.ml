open! Core
open Async
open Bonsai_term
open Bonsai.Let_syntax

let app ~dimensions (local_ graph) =
  let flavor = Bonsai_tui_catpuccin.flavor graph in
  let%sub { view; handler; string = _; set = _ } =
    let text_attrs =
      let%arr flavor in
      [ Attr.bg (Bonsai_tui_catpuccin.color ~flavor Crust)
      ; Attr.fg (Bonsai_tui_catpuccin.color ~flavor Text)
      ]
    and cursor_attrs =
      let%arr flavor in
      [ Attr.bg (Bonsai_tui_catpuccin.color ~flavor Text)
      ; Attr.fg (Bonsai_tui_catpuccin.color ~flavor Crust)
      ]
    in
    Bonsai_tui_textbox.component
      ~text_attrs
      ~cursor_attrs
      ~is_focused:(Bonsai.return true)
      graph
  in
  let view =
    let%arr view
    and flavor
    and terminal_dimensions = dimensions in
    let _ = terminal_dimensions in
    let spacer color =
      View.text ~attrs:[ Attr.bg (Bonsai_tui_catpuccin.color ~flavor color) ] " "
    in
    let textbox = View.hcat [ spacer Crust; view; spacer Crust ]
    and label =
      View.text
        ~attrs:
          [ Attr.bold
          ; Attr.fg (Bonsai_tui_catpuccin.color ~flavor Green)
          ; Attr.bg (Bonsai_tui_catpuccin.color ~flavor Surface0)
          ]
        " Your name: "
    in
    View.pad ~l:2 ~t:1 (View.hcat [ label; textbox ])
  in
  ~view, ~handler
;;

let command =
  Command.async_or_error ~summary:{|Demo of the textbox component!|}
  @@
  let%map_open.Command () = return () in
  fun () ->
    let open Deferred.Or_error.Let_syntax in
    let%bind () = start app in
    return ()
;;

let () = Command_unix.run command
