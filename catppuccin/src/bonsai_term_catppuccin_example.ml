open! Core
open! Async
open! Bonsai_term
open Bonsai.Let_syntax
module Catppuccin = Bonsai_tui_catppuccin

let all_colors (local_ graph) =
  Bonsai.map (Catppuccin.flavor graph) ~f:(fun flavor ->
    Catppuccin.all
    |> List.map ~f:(fun color -> color, Catppuccin.color ~flavor color)
    |> Catppuccin.Map.of_alist_exn)
;;

let all_flavors = Core.Array.of_list Catppuccin.Flavor.all

let app ~dimensions (local_ graph) =
  let index, cycle =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ mode -> function
        | `Left -> mode - 1
        | `Right -> mode + 1)
      graph
  in
  let handler =
    let%arr cycle in
    fun (event : Event.t) ->
      match event with
      | Key_press { key = ASCII 'h' | Arrow `Left; mods = [] } -> cycle `Left
      | Key_press { key = ASCII 'l' | Arrow `Right; mods = [] } -> cycle `Right
      | Key_press { key = Tab; mods = [] } -> cycle `Right
      | Key_press { key = Tab; mods = [ Shift ] } -> cycle `Left
      | _ -> Effect.Ignore
  in
  let flavor =
    let%arr index in
    all_flavors.(index % Array.length all_flavors)
  in
  let biggest_color_length =
    List.max_elt
      ~compare:[%compare: int]
      (List.map Catppuccin.all ~f:(fun x ->
         String.length (Sexp.to_string [%sexp (x : Catppuccin.t)])))
    |> Option.value ~default:8
  in
  let view =
    (Catppuccin.set_flavor_within flavor
     @@ fun (local_ graph) ->
     let all_colors = all_colors graph in
     let%arr all_colors and flavor and dimensions in
     let title =
       Catppuccin.Flavor.all
       |> List.map ~f:(fun f ->
         let attrs =
           [ Attr.bg
               (Catppuccin.color
                  ~flavor
                  (if Catppuccin.Flavor.equal flavor f then Catppuccin.Mauve else Base))
           ; Attr.fg
               (Catppuccin.color
                  ~flavor
                  (if Catppuccin.Flavor.equal flavor f then Catppuccin.Base else Text))
           ]
         in
         View.text ~attrs (" " ^ Sexp.to_string [%sexp (f : Catppuccin.Flavor.t)] ^ " "))
       |> View.hcat
     in
     let colors =
       View.vcat
       @@ List.map (Core.Map.to_alist all_colors) ~f:(fun (color, color_attr) ->
         View.hcat
           [ View.text ~attrs:[ Attr.bg color_attr ] "  "
           ; View.text
               ~attrs:[ Attr.fg color_attr ]
               (String.pad_right
                  ~len:(biggest_color_length + 2)
                  (" " ^ Sexp.to_string [%sexp (color : Catppuccin.t)] ^ " "))
           ])
     in
     let text_attrs =
       [ Attr.fg (Catppuccin.color ~flavor Text)
       ; Attr.bg (Catppuccin.color ~flavor Base)
       ]
     in
     let content =
       View.pad
         ~l:2
         ~t:1
         (View.vcat
            [ View.hcat
                [ View.text
                    ~attrs:
                      (text_attrs
                       @ [ Attr.bold; Attr.fg (Catppuccin.color ~flavor Mauve) ])
                    " Catppuccin "
                ; View.text ~attrs:text_attrs "colors. Press "
                ; View.text
                    ~attrs:
                      [ Attr.fg (Catppuccin.color ~flavor Mauve)
                      ; Attr.bg (Catppuccin.color ~flavor Base)
                      ; Attr.bold
                      ]
                    "<tab>"
                ; View.text ~attrs:text_attrs " to change flavor."
                ]
            ; title
            ; View.text ""
            ; View.pad ~l:2 colors
            ])
     in
     View.zcat
       [ content
       ; View.rectangle
           ~attrs:[ Attr.bg (Catppuccin.color ~flavor Base) ]
           ~width:dimensions.Dimensions.width
           ~height:dimensions.Dimensions.height
           ()
       ])
      graph
  in
  ~view, ~handler
;;

let command =
  Command.async_or_error
    ~summary:{|Catppuccin colors demo!|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;
