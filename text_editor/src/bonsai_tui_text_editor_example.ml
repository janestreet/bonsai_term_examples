open! Core
open Bonsai_term
open Bonsai.Let_syntax

let toggle_keybindings_button_tag =
  View.Tag.create
    (module Unit)
    ~reduce:(fun _ t -> t)
    ~transform_regions:(fun region f -> f region)
;;

let app ?(initial_keybindings = `Standard) ~dimensions (local_ graph) =
  let flavor = Bonsai_tui_catpuccin.flavor graph in
  let%tydi { view; send_actions; text; cursor; set_text; rope = _; get_cursor_position } =
    let text_attrs =
      let%arr flavor in
      let text_attrs =
        [ Attr.bg (Bonsai_tui_catpuccin.color ~flavor Crust)
        ; Attr.fg (Bonsai_tui_catpuccin.color ~flavor Text)
        ]
      in
      text_attrs
    in
    let width =
      let%arr { Dimensions.width; _ } = dimensions in
      width
    in
    let max_height =
      let%arr { Dimensions.height; _ } = dimensions in
      height - 1
    in
    Bonsai_tui_text_editor.component ~text_attrs ~width ~max_height graph
  in
  let keybindings, toggle =
    Bonsai.state_machine
      ~default_model:initial_keybindings
      ~apply_action:(fun _ model () ->
        match model with
        | `Standard -> `Vim
        | `Vim -> `Emacs
        | `Emacs -> `Standard)
      graph
  in
  let%sub ~handler, ~mode =
    match%sub keybindings with
    | `Standard ->
      let handler =
        let%arr send_actions in
        Bonsai_tui_text_editor.default_keybindings_handler send_actions
      in
      let handler =
        Bonsai_tui_text_editor.Buffer_and_apply_paste_events_in_bulk.f
          ~send_actions
          ~handler
          graph
      in
      let%arr handler in
      ~handler, ~mode:None
    | `Vim ->
      let%tydi { mode; handler } =
        Bonsai_tui_text_editor.Vim.vim_keybindings_handler
          ~default_mode:Normal
          send_actions
          graph
      in
      let handler =
        Bonsai_tui_text_editor.Buffer_and_apply_paste_events_in_bulk.f
          ~send_actions
          ~handler
          graph
      in
      let%arr handler and mode in
      ~handler, ~mode:(Some mode)
    | `Emacs ->
      let handler =
        Bonsai_tui_text_editor.Emacs.emacs_keybindings_handler send_actions graph
      in
      let handler =
        Bonsai_tui_text_editor.Buffer_and_apply_paste_events_in_bulk.f
          ~send_actions
          ~handler
          graph
      in
      let%arr handler in
      ~handler, ~mode:None
  in
  let view =
    let%sub mode, color =
      let%arr mode and flavor in
      let text, color =
        match mode with
        | Some Normal -> " NORMAL ", Bonsai_tui_catpuccin.Blue
        | Some Insert -> " INSERT ", Bonsai_tui_catpuccin.Green
        | None -> "", Bonsai_tui_catpuccin.Mauve
      in
      let view =
        View.text
          ~attrs:
            [ Attr.bold
            ; Attr.fg (Bonsai_tui_catpuccin.color ~flavor Crust)
            ; Attr.bg (Bonsai_tui_catpuccin.color ~flavor color)
            ]
          text
      in
      view, color
    in
    let bar =
      let%arr mode
      and { visual_line; visual_column; logical_column; logical_line; position = _ } =
        cursor
      and { width; height = _ } = dimensions
      and color
      and flavor
      and keybindings in
      let visual_cursor =
        View.text
          ~attrs:
            [ Attr.fg (Bonsai_tui_catpuccin.color ~flavor color)
            ; Attr.bg (Bonsai_tui_catpuccin.color ~flavor Surface0)
            ]
          [%string " %{visual_line#Int}:%{visual_column#Int} "]
      in
      let cursor =
        View.text
          ~attrs:
            [ Attr.fg (Bonsai_tui_catpuccin.color ~flavor Crust)
            ; Attr.bg (Bonsai_tui_catpuccin.color ~flavor color)
            ]
          [%string " %{logical_line#Int}:%{logical_column#Int} "]
      in
      let right = View.hcat [ visual_cursor; cursor ] in
      let keybindings =
        let name =
          match keybindings with
          | `Standard -> " standard keybindings "
          | `Vim -> " vim keybindings "
          | `Emacs -> " emacs keybindings "
        in
        let node =
          View.hcat
            [ View.text
                ~attrs:
                  [ Attr.bold
                  ; Attr.bg (Bonsai_tui_catpuccin.color ~flavor Surface0)
                  ; Attr.fg (Bonsai_tui_catpuccin.color ~flavor Yellow)
                  ]
                name
            ; View.text
                ~attrs:
                  [ Attr.bg (Bonsai_tui_catpuccin.color ~flavor Base)
                  ; Attr.fg (Bonsai_tui_catpuccin.color ~flavor Overlay2)
                  ]
                " click to toggle "
            ]
        in
        View.Tag.mark node ~id:toggle_keybindings_button_tag ~key:() ~f:Fn.id
      in
      let left = View.hcat [ mode; keybindings ] in
      let middle_padding = Int.max 0 (width - View.width left - View.width right) in
      let middle =
        View.rectangle
          ~attrs:[ Attr.bg (Bonsai_tui_catpuccin.color ~flavor Mantle) ]
          ~width:middle_padding
          ~height:1
          ()
      in
      View.hcat [ left; middle; right ]
    in
    let%arr view and bar in
    View.vcat [ view; bar ]
  in
  let view =
    let%arr view
    and { Dimensions.width; height } = dimensions
    and flavor in
    let backdrop =
      View.rectangle
        ~attrs:[ Attr.bg (Bonsai_tui_catpuccin.color ~flavor Crust) ]
        ~width
        ~height
        ()
    in
    View.zcat [ view; backdrop ]
  in
  let () =
    let update_cursor_position =
      let set_cursor = Effect.set_cursor graph in
      let%arr view and set_cursor and mode and get_cursor_position in
      let cursor_kind =
        match mode with
        | Some Normal -> Cursor.Kind.Block
        | Some Insert -> Bar_blinking
        | None -> Bar_blinking
      in
      match get_cursor_position view with
      | None -> set_cursor None
      | Some { x; y } -> set_cursor (Some { position = { x; y }; kind = cursor_kind })
    in
    Bonsai.Edge.after_display update_cursor_position graph
  in
  let handler =
    let (last_click : (unit, Region.t) View.Tag.t option Bonsai.t), set_last_click =
      Bonsai.state None graph
    in
    let%arr toggle
    and view = Bonsai.peek view graph
    and last_click = Bonsai.peek last_click graph
    and set_last_click
    and handler in
    let with_view_and_last_click f =
      match%bind.Effect view with
      | Inactive -> Effect.Ignore
      | Active view ->
        (match%bind.Effect last_click with
         | Inactive -> Effect.Ignore
         | Active last_click -> f ~view ~last_click)
    in
    fun (event : Event.t) ->
      match event with
      | Mouse { kind = Left; position; mods = [] } ->
        with_view_and_last_click
        @@ fun ~view ~last_click:_ ->
        (match View.Tag.find view ~id:toggle_keybindings_button_tag () with
         | None -> set_last_click None
         | Some region ->
           if Region.contains region position
           then set_last_click (Some toggle_keybindings_button_tag)
           else set_last_click None)
      | Mouse { kind = Release; position; mods = [] } ->
        with_view_and_last_click
        @@ fun ~view ~last_click ->
        (match last_click with
         | None -> Effect.Ignore
         | Some last_click ->
           (match View.Tag.find view ~id:last_click () with
            | None -> set_last_click None
            | Some region ->
              let%bind.Effect () =
                if Region.contains region position then toggle () else Effect.Ignore
              in
              set_last_click None))
      | _ -> handler event
  in
  let toggle_keybindings_mode =
    let%arr toggle in
    toggle ()
  in
  ~view, ~handler, ~toggle_keybindings_mode, ~text, ~set_text, ~get_cursor_position
;;
