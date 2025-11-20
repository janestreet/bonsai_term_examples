open! Core
open! Bonsai_term
open Bonsai.Let_syntax

module Scroll_region : sig
  module Focus : sig
    type t

    val make : local_ Bonsai.graph -> t Bonsai.t
    val tag : View.t -> t -> View.t
  end

  val create
    :  focus:Focus.t Bonsai.t
    -> height:int Bonsai.t
    -> View.t Bonsai.t
    -> local_ Bonsai.graph
    -> View.t Bonsai.t
end = struct
  module Focus = struct
    type t = Bonsai.Path.t

    let id : (Bonsai.Path.t, Region.t) View.Tag.t =
      View.Tag.create
        (module Bonsai.Path)
        ~transform_regions:(fun region f -> f region)
        ~reduce:(fun _ t -> t)
    ;;

    let make graph = Bonsai.path graph
    let tag node path = View.Tag.mark node ~id ~key:path ~f:(fun region -> region)
  end

  (* We assume that [content] has been tagged with the [focus] tag. *)
  let create ~focus ~height content (local_ graph) =
    (* Track the current offset so that when you move focus down and then go back up, the
       scrollbar remains locked until focus moves out of bounds in the "up" direction. *)
    let scroll_offset, set_scroll_offset = Bonsai.state 0 graph in
    let position_of_tag =
      (* try to find the position of the tag in the overall list *)
      Bonsai.cutoff
        ~equal:[%equal: Region.t option]
        (let%arr content and focus in
         let%map.Option location = View.Tag.find content ~id:Focus.id focus in
         location)
    in
    let adjusted_scroll_offset =
      (* Compute the adjusted scroll offset so that the scroller moves on the same frame
         that the tag leaves the scroll region. If we _just_ relied on the [on_change]
         then it would be delayed by a frame. *)
      let%arr scroll_offset and position_of_tag and height in
      match position_of_tag with
      | None -> scroll_offset
      | Some { y; height = h; _ } ->
        let scroll_offset =
          let local_offset = y - height + h in
          if local_offset > scroll_offset then local_offset else scroll_offset
        in
        let scroll_offset = if y < scroll_offset then y else scroll_offset in
        scroll_offset
    in
    Bonsai.Edge.on_change
      adjusted_scroll_offset
      ~equal:[%equal: int]
      ~callback:set_scroll_offset
      graph;
    let%arr height and content and focus and adjusted_scroll_offset in
    let view = View.crop ~t:adjusted_scroll_offset content in
    let b_crop = Int.max 0 (View.height content - height - adjusted_scroll_offset) in
    view |> View.Tag.remove ~id:Focus.id ~key:focus |> View.crop ~b:b_crop
  ;;
end

let items =
  [ "Capybara"
  ; "Squirrel"
  ; "Hamster"
  ; "Chinchilla"
  ; "Beaver"
  ; "Corgi"
  ; "Basset Hound"
  ; "Ants"
  ; "Birch"
  ; "Oak"
  ; "Acacia"
  ; "Spruce"
  ]
;;

let app ~dimensions:_ (local_ graph) =
  let focus_idx, inject =
    Bonsai.state_machine_with_input
      ~default_model:0
      ~apply_action:(fun _ input idx action ->
        match input with
        | Inactive -> idx
        | Active size ->
          (match action with
           | `Up -> Int.max 0 (idx - 1)
           | `Down -> Int.min (size - 1) (idx + 1)))
      (Bonsai.return (List.length items))
      graph
  in
  let focus = Scroll_region.Focus.make graph in
  let content =
    let%arr focus_idx and focus in
    List.mapi items ~f:(fun i name ->
      let node = View.text name in
      if i = focus_idx
      then View.hcat [ View.text "> "; Scroll_region.Focus.tag node focus ]
      else View.hcat [ View.text "  "; node ])
    |> View.vcat
  in
  let content = Scroll_region.create ~focus ~height:(Bonsai.return 5) content graph in
  let handler =
    let%arr inject in
    fun (event : Event.t) ->
      match event with
      | Event.Key_press { key = Arrow `Down | ASCII 'j'; _ } -> inject `Down
      | Event.Key_press { key = Arrow `Up | ASCII 'k'; _ } -> inject `Up
      | _ -> Effect.Ignore
  in
  ~view:content, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:
      {|A demo of a scrollable list component that uses tags to automatically scroll when the item goes out of view|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;

let () = Command_unix.run command
