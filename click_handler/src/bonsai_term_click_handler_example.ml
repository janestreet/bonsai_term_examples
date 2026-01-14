open! Core
open Async
open! Bonsai_term
open Bonsai.Let_syntax

let bg = Bonsai_tui_catppuccin.Crust

let backdrop ~dimensions (local_ graph) =
  let%arr { Dimensions.height; width } = dimensions
  and flavor = Bonsai_tui_catppuccin.flavor graph in
  let bg_color = Bonsai_tui_catppuccin.color ~flavor bg in
  View.rectangle ~height ~width ~attrs:[ Attr.bg bg_color ] ()
;;

let text (local_ graph) =
  let%arr flavor = Bonsai_tui_catppuccin.flavor graph in
  let text_color = Bonsai_tui_catppuccin.color ~flavor Text in
  let crust = Bonsai_tui_catppuccin.color ~flavor bg in
  fun ?(attrs = []) text ->
    View.text ~attrs:([ Attr.fg text_color; Attr.bg crust ] @ attrs) text
;;

let button ~label ~on_click (local_ graph) =
  let add_click_handler = Bonsai_tui_click_handler.add_click_handler graph in
  let%arr add_click_handler
  and on_click
  and text = text graph
  and flavor = Bonsai_tui_catppuccin.flavor graph in
  let mauve = Bonsai_tui_catppuccin.color ~flavor Mauve in
  let button_view =
    View.hcat
      [ text ~attrs:[ Attr.bg mauve ] " "
      ; text
          ~attrs:
            [ Attr.bg mauve
            ; Attr.fg (Bonsai_tui_catppuccin.color ~flavor Crust)
            ; Attr.bold
            ]
          label
      ; text ~attrs:[ Attr.bg mauve ] " "
      ]
  in
  add_click_handler button_view ~on_click
;;

let counter_component (local_ graph) =
  let count, inject = Bonsai.state 0 graph in
  let increment_button =
    let on_click =
      let%arr inject and count in
      inject (count + 1)
    in
    button ~label:"+" ~on_click graph
  in
  let decrement_button =
    let on_click =
      let%arr inject and count in
      inject (count - 1)
    in
    button ~label:"-" ~on_click graph
  in
  let reset_button =
    let on_click =
      let%arr inject in
      inject 0
    in
    button ~label:"Reset" ~on_click graph
  in
  let%arr count
  and increment_button
  and decrement_button
  and reset_button
  and text = text graph
  and flavor = Bonsai_tui_catppuccin.flavor graph in
  let green = Bonsai_tui_catppuccin.color ~flavor Green in
  let subtext = Bonsai_tui_catppuccin.color ~flavor Subtext0 in
  View.vcat
    [ text ~attrs:[ Attr.bold; Attr.fg green ] "Click Handler Demo"
    ; View.text ""
    ; View.hcat [ text "Count: "; text ~attrs:[ Attr.bold ] (Int.to_string count) ]
    ; View.text ""
    ; View.hcat [ increment_button; text "  "; decrement_button; text "  "; reset_button ]
    ; View.text ""
    ; text ~attrs:[ Attr.fg subtext ] "(Click the buttons with your mouse!)"
    ]
;;

let app ~dimensions (local_ graph) =
  let counter_view = counter_component graph in
  let view =
    let%arr backdrop = backdrop ~dimensions graph
    and counter_view in
    View.zcat [ View.pad ~l:2 ~t:1 counter_view; backdrop ]
  in
  let handler =
    Bonsai_tui_click_handler.handler
      ~view
      ~handler:(Bonsai.return (fun _event -> Effect.Ignore))
      graph
  in
  ~view, ~handler
;;

let command =
  Command.async_or_error
    ~summary:{|Click handler example - demonstrates mouse click handling in bonsai_term|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;
