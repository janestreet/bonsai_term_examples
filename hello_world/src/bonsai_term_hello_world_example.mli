(*_ Intentionally left empty. *)
open! Core
open! Bonsai_term

val app
  :  dimensions:Dimensions.t Bonsai.t
  -> Bonsai.graph @ local
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

val command : Command.t
