open! Core
open! Bonsai_term
open Bonsai.Let_syntax
module Catppuccin = Bonsai_tui_catppuccin

let app ~(dimensions : Dimensions.t Bonsai.t) (local_ graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let flavor = Catppuccin.flavor graph in
  let attrs =
    let%arr flavor in
    [ Attr.bold; Attr.fg (Catppuccin.color ~flavor Green) ]
  in
  let dot_spinner = Bonsai_term_spinner.component ~kind:Dot ~attrs graph in
  let line_spinner = Bonsai_term_spinner.component ~kind:Line ~attrs graph in
  let content =
    let%arr dot_spinner and line_spinner and flavor in
    let kind = View.text ~attrs:[ Attr.fg (Catppuccin.color ~flavor Pink) ] "~kind:" in
    let value s =
      View.text
        ~attrs:
          [ Attr.bold
          ; Attr.fg (Catppuccin.color ~flavor Mauve)
          ; Attr.fg (Catppuccin.color ~flavor Mauve)
          ]
        s
    in
    let kind name =
      View.with_colors'
        ~fill_backdrop:true
        ~bg:(Catppuccin.color ~flavor Base)
        (View.hcat
           [ View.rectangle ~height:1 ~width:1 ()
           ; kind
           ; value name
           ; View.rectangle ~height:1 ~width:1 ()
           ])
    in
    let arrow = View.text ~attrs:[ Attr.bg (Catppuccin.color ~flavor Mantle) ] " -> " in
    let pad view =
      let padding = View.rectangle ~height:1 ~width:1 () in
      View.with_colors'
        ~bg:(Catppuccin.color ~flavor Base)
        (View.hcat [ padding; view; padding ])
    in
    View.vcat
      [ pad
          (View.hcat
             [ View.text
                 ~attrs:[ Attr.fg (Catppuccin.color ~flavor Mauve) ]
                 "Bonsai_term_spinner"
             ; View.text ".component"
             ])
      ; View.text ""
      ; View.hcat [ pad dot_spinner; arrow; kind "Dot" ]
      ; View.hcat [ pad line_spinner; arrow; kind "Line" ]
      ]
  in
  let backdrop =
    let%arr flavor
    and { width; height } = dimensions in
    View.rectangle ~width ~height ~attrs:[ Attr.bg (Catppuccin.color ~flavor Crust) ] ()
  in
  let view =
    let%arr dimensions and content and flavor and backdrop in
    View.zcat
      [ View.with_colors
          ~bg:(Catppuccin.color ~flavor Crust)
          ~fg:(Catppuccin.color ~flavor Text)
          (View.center ~within:dimensions content)
      ; backdrop
      ]
  in
  let handler = Bonsai.return (fun _ -> Effect.Ignore) in
  ~view, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:{|Spinner example|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;

let () = Command_unix.run command
