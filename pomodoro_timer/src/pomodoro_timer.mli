open! Core
open! Bonsai_term

(** The main pomodoro timer application *)
val app
  :  dimensions:Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
