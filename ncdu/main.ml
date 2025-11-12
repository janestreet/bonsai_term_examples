open! Core

module Classification = struct
  type t =
    | Kingdom of string
    | Family of string
    | Species of string
  [@@deriving sexp_of, compare]

  include functor Comparable.Make_plain

  let to_string_hum = function
    | Kingdom s | Family s | Species s -> s
  ;;

  let catpuccin_color = function
    | Kingdom _ -> Some Bonsai_tui_catpuccin.Blue
    | Family _ -> Some Lavender
    | Species _ -> None
  ;;
end

include Bonsai_tui_ncdu.Make (Classification) (Int)

let species name count =
  let name = Classification.Species name in
  let node =
    { Tree_node.name
    ; weight = { dominated = count; self = count }
    ; children = Classification.Map.empty
    }
  in
  name, node
;;

let generic_node_without_self_weight ~create name children =
  let name = create name in
  let dominated_weight =
    List.sum (module Int) children ~f:(fun (_, node) -> node.Tree_node.weight.dominated)
  in
  let node =
    { Tree_node.name
    ; weight = { dominated = dominated_weight; self = 0 }
    ; children = Classification.Map.of_alist_exn children
    }
  in
  name, node
;;

let family =
  generic_node_without_self_weight ~create:(fun name -> Classification.Family name)
;;

let kingdom =
  generic_node_without_self_weight ~create:(fun name -> Classification.Kingdom name)
;;

let nodes =
  Classification.Map.of_alist_exn
    [ kingdom
        "Animals"
        [ family
            "Rodents"
            [ species "Capybara" 10
            ; species "Squirrel" 8
            ; species "Hamster" 2
            ; species "Chinchilla" 1
            ; species "Beaver" 0
            ]
        ; family "Canidae" [ species "Corgi" 10; species "Basset Hound" 12 ]
        ; family "Formicidae" [ species "Ants" 100 ]
        ]
    ; kingdom
        "Plants"
        [ family
            "Trees"
            [ species "Birch" 10
            ; species "Oak" 2
            ; species "Acacia" 19
            ; species "Spruce" 12
            ]
        ]
    ; kingdom "Fungi" []
    ]
;;

let total_size =
  Map.sum (module Int) nodes ~f:(fun node -> node.Tree_node.weight.dominated)
;;

let app ~dimensions (local_ graph) =
  component
    ~dimensions
    ~app_title:"Tree of life"
    ~tree_name:"South America"
    ~nodes
    ~total_size
    graph
;;

let () =
  let open Async in
  (let%map_open.Command () = return () in
   fun () ->
     let%bind.Deferred.Or_error () = Bonsai_term.start ~target_frames_per_second:10 app in
     Deferred.Or_error.return ())
  |> Command.async_or_error ~summary:{|Bonsai_term ncdu demo!|}
  |> Command_unix.run
;;
