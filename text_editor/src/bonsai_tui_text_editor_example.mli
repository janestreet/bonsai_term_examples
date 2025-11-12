open! Core
open Bonsai_term

val app
  :  ?initial_keybindings:[ `Standard | `Vim | `Emacs ]
  -> dimensions:Dimensions.t Bonsai_term.Bonsai.t
  -> local_ Bonsai_term.Bonsai.graph
  -> view:View.t Bonsai_term.Bonsai.t
     * handler:(Event.t -> unit Bonsai_term.Effect.t) Bonsai_term.Bonsai.t
     * toggle_keybindings_mode:unit Effect.t Bonsai.t
     * text:string Bonsai.t
     * set_text:(string -> unit Effect.t) Bonsai.t
     * get_cursor_position:(View.t -> Position.t option) Bonsai.t
