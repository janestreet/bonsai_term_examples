open! Core
open! Bonsai_term
open Bonsai.Let_syntax

let green x =
  View.text
    ~attrs:[ Attr.fg (Bonsai_tui_catpuccin.color ~flavor:Mocha Green); Attr.bold ]
    x
;;

let render_mod mods f name = if List.exists ~f mods then name ^ "+" else ""

let render_event : Event.t -> View.t =
  fun event ->
  match event with
  | Key_press { key; mods } ->
    let meta =
      render_mod
        mods
        (function
          | Meta -> true
          | _ -> false)
        "Meta"
    and ctrl =
      render_mod
        mods
        (function
          | Ctrl -> true
          | _ -> false)
        "Ctrl"
    and shift =
      render_mod
        mods
        (function
          | Shift -> true
          | _ -> false)
        "Shift"
    in
    let prefix = String.concat [ meta; ctrl; shift ] in
    let key =
      match key with
      | ASCII ' ' -> "SPACE"
      | ASCII c -> Char.to_string c
      | Uchar uchar -> Uchar.Utf8.to_string uchar
      | ( Escape
        | Enter
        | Tab
        | Backspace
        | Insert
        | Delete
        | Home
        | End
        | Arrow _
        | Page _
        | Function _ ) as event -> Sexp.to_string_mach [%sexp (event : Event.Key.t)]
    in
    View.hcat
      [ green "Key "
      ; View.text
          ~attrs:[ Attr.bold; Attr.fg (Bonsai_tui_catpuccin.color ~flavor:Mocha Blue) ]
          (prefix ^ key)
      ]
  | Mouse { kind; position; mods } ->
    View.hcat
      [ green "Mouse "
      ; View.text
          ~attrs:[ Attr.bold; Attr.fg (Bonsai_tui_catpuccin.color ~flavor:Mocha Blue) ]
          (Sexp.to_string
             [%message
               (kind : Event.mouse_kind)
                 (position : Position.t)
                 (mods : Event.Modifier.t list)])
      ]
  | Paste `Start -> View.hcat [ green "Paste (start)" ]
  | Paste `End -> View.hcat [ green "Paste (end)" ]
;;

let app ~dimensions:_ (local_ graph) =
  let events, add_event =
    Bonsai.state_machine
      ~default_model:[]
      ~apply_action:(fun _ events event -> event :: List.take events 30)
      graph
  in
  let view =
    let%arr events in
    View.vcat (View.text "Press some keys!" :: List.map events ~f:render_event)
  in
  ~view, ~handler:add_event
;;

let command =
  Async.Command.async_or_error
    ~summary:{|A that shows the received [Event.t]'s|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;

let () = Command_unix.run command
