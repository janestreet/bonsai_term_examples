open! Core
open Bonsai_term

val app
  :  ?initial_keybindings:[ `Standard | `Vim | `Emacs ]
  -> dimensions:Dimensions.t Bonsai_term.Bonsai.t
  -> local_ Bonsai_term.Bonsai.graph
  -> view:View.t Bonsai.t
     * handler:(Event.t -> unit Bonsai_term.Effect.t) Bonsai.t
     * toggle_keybindings_mode:unit Effect.t Bonsai.t
     * text:string Bonsai.t
     * set_text:(string -> unit Effect.t) Bonsai.t
     * mode:Bonsai_tui_text_editor.Vim.Mode.t option Bonsai.t
     * get_cursor_position:(View.t -> Position.t option) Bonsai.t

val register_cursor
  :  view:View.t Bonsai.t
  -> mode:Bonsai_tui_text_editor.Vim.Mode.t option Bonsai.t
  -> get_cursor_position:(View.t -> Position.t option) Bonsai.t
  -> Bonsai.graph @ local
  -> unit

val command : Command.t
