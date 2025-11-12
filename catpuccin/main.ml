open! Core
open! Async
open! Bonsai_term
open Bonsai.Let_syntax
module Catpuccin = Bonsai_tui_catpuccin

let all_colors (local_ graph) =
  Bonsai.map (Catpuccin.flavor graph) ~f:(fun flavor ->
    Catpuccin.all
    |> List.map ~f:(fun color -> color, Catpuccin.color ~flavor color)
    |> Catpuccin.Map.of_alist_exn)
;;

let all_flavors = Core.Array.of_list Catpuccin.Flavor.all

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
      (List.map Catpuccin.all ~f:(fun x ->
         String.length (Sexp.to_string [%sexp (x : Catpuccin.t)])))
    |> Option.value ~default:8
  in
  let view =
    (Catpuccin.set_flavor_within flavor
     @@ fun (local_ graph) ->
     let all_colors = all_colors graph in
     let%arr all_colors and flavor and dimensions in
     let title =
       Catpuccin.Flavor.all
       |> List.map ~f:(fun f ->
         let attrs =
           [ Attr.bg
               (Catpuccin.color
                  ~flavor
                  (if Catpuccin.Flavor.equal flavor f then Catpuccin.Mauve else Base))
           ; Attr.fg
               (Catpuccin.color
                  ~flavor
                  (if Catpuccin.Flavor.equal flavor f then Catpuccin.Base else Text))
           ]
         in
         View.text ~attrs (" " ^ Sexp.to_string [%sexp (f : Catpuccin.Flavor.t)] ^ " "))
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
                  (" " ^ Sexp.to_string [%sexp (color : Catpuccin.t)] ^ " "))
           ])
     in
     let text_attrs =
       [ Attr.fg (Catpuccin.color ~flavor Text); Attr.bg (Catpuccin.color ~flavor Base) ]
     in
     let content =
       View.pad
         ~l:2
         ~t:1
         (View.vcat
            [ View.hcat
                [ View.text
                    ~attrs:
                      (text_attrs @ [ Attr.bold; Attr.fg (Catpuccin.color ~flavor Mauve) ])
                    " Catpuccin "
                ; View.text ~attrs:text_attrs "colors. Press "
                ; View.text
                    ~attrs:
                      [ Attr.fg (Catpuccin.color ~flavor Mauve)
                      ; Attr.bg (Catpuccin.color ~flavor Base)
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
           ~attrs:[ Attr.bg (Catpuccin.color ~flavor Base) ]
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
    ~summary:{|Catpuccin colors demo!|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;

let () = Command_unix.run command
