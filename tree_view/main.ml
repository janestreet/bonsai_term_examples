open! Core
open Async
open Bonsai_term
open Bonsai.Let_syntax
module Tree_view = Bonsai_tui_tree_view

let grey = Attr.Color.rgb ~r:150 ~g:150 ~b:150

module Tree = struct
  type t =
    { component : string
    ; added_directly : bool
    ; children : t String.Map.t
    }

  let empty = { component = ""; added_directly = false; children = String.Map.empty }

  let rec add t = function
    | [] -> { t with added_directly = true }
    | component :: rest ->
      let children =
        Map.update t.children component ~f:(function
          | None ->
            let child = { empty with component } in
            add child rest
          | Some child -> add child rest)
      in
      { t with children }
  ;;

  let rec to_tree_view { component; added_directly; children } =
    let attrs = if added_directly then [ Attr.bold ] else [ Attr.fg grey ] in
    if Map.is_empty children
    then Tree_view.Leaf (View.text ~attrs component)
    else
      Tree_view.Branch
        (View.text ~attrs component, Map.data children |> List.map ~f:to_tree_view)
  ;;

  let to_tree_view root =
    match Map.length root.children with
    | 0 -> Tree_view.Split []
    | 1 ->
      let _, root = Map.min_elt_exn root.children in
      to_tree_view root
    | _ -> Tree_view.Split (Map.data root.children |> List.map ~f:to_tree_view)
  ;;
end

let app ~dimensions:_ ~paths (local_ _graph) =
  let paths_in_tree =
    let%arr paths in
    paths
    |> List.fold ~init:Tree.empty ~f:(fun acc path ->
      Tree.add acc (String.split path ~on:'/'))
  in
  let view =
    let%arr paths_in_tree in
    paths_in_tree |> Tree.to_tree_view |> Tree_view.render ~layout_attrs:[ Attr.fg grey ]
  in
  ~view, ~handler:(Bonsai.return (fun _ -> Effect.Ignore))
;;

let command =
  let open Deferred.Or_error.Let_syntax in
  Command.async_or_error
    ~summary:
      {|Demo of bonsai_tui_tree_view.  To use, pass a file containing newline separated paths.
e.g. [./main.exe <(find ../ -type f)]
      |}
  @@
  let%map_open.Command files = anon (Command.Anons.sequence ("FILE" %: string)) in
  fun () ->
    let paths_var = Bonsai.Expert.Var.create [] in
    let%bind () =
      Deferred.Or_error.List.iter files ~how:`Parallel ~f:(fun file ->
        let%bind file = Deferred.ok (Reader.open_file file) in
        file
        |> Async.Reader.lines
        |> Pipe.iter_without_pushback ~f:(fun line ->
          Bonsai.Expert.Var.update paths_var ~f:(fun paths -> line :: paths))
        |> Deferred.ok)
    in
    let%bind () =
      Bonsai_term.start (fun ~dimensions graph ->
        app ~dimensions ~paths:(Bonsai.Expert.Var.value paths_var) graph)
    in
    return ()
;;

let () = Command_unix.run command
