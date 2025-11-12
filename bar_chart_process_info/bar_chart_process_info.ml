open! Core
open! Bonsai_term
open! Bonsai_tui_bar_chart
open! Bonsai.Let_syntax

(* This is a tiny example that shows CPU utilization of different processes on your box
   with an TUI Bar chart. *)

module Process_info = struct
  type t =
    { name : string
    ; cpu : Percent.t
    }
  [@@deriving equal, sexp]

  let ps () =
    Effect.of_deferred_thunk (fun () ->
      Async.Process.run_lines ~prog:"ps" ~args:[ "aux"; "--no-headers" ] ())
  ;;

  let parse_process_info_line line =
    let get_name command =
      let last_slash =
        String.rfindi command ~f:(fun _i c -> Char.equal c '/')
        |> Option.map ~f:(fun n -> n + 1)
        |> Option.value ~default:0
      in
      String.sub command ~pos:last_slash ~len:(String.length command - last_slash)
    in
    match String.split line ~on:' ' |> List.filter ~f:(fun s -> String.length s > 0) with
    | _user
      :: _pid
      :: cpu
      :: _mem
      :: _vsz
      :: _rss
      :: _tty
      :: _stat
      :: _start
      :: _time
      :: command ->
      { name = get_name (List.hd_exn command); cpu = Percent.of_string (cpu ^ "%") }
    | line -> raise_s [%message "Invalid line" (line : string list)]
  ;;

  let get_sorted ?(n = 6) () : t list Effect.t =
    let%map.Effect lines = ps () in
    let lines = Or_error.ok lines |> Option.value ~default:[] in
    let lines =
      List.map ~f:parse_process_info_line lines
      |> List.sort ~compare:(fun t1 t2 -> Percent.compare t2.cpu t1.cpu)
    in
    List.take lines n
  ;;
end

let make_view ~(dimensions : Dimensions.t Bonsai.t) (local_ graph) =
  let bar_width = 10 in
  let bar_padding = 2 in
  let process_info, set_process_info = Bonsai.state [] graph in
  let () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
      ~trigger_on_activate:true
      (Bonsai.return (Time_ns.Span.of_sec 0.1))
      (let%arr set_process_info
       and { width; height = _ } = dimensions in
       let total_width_per_bar = bar_width + (2 * bar_padding) in
       (* we can't fill the full [width] of the [dimensions] with bars because we need to
          save room for the axis and labels. *)
       let num_bars = (width - 10) / total_width_per_bar in
       let%bind.Effect process_info = Process_info.get_sorted ~n:num_bars () in
       set_process_info process_info)
      graph
  in
  (* This is the interesting part of the example where the graph is actually created. *)
  let bars =
    let%arr process_info in
    List.map process_info ~f:(fun { name; cpu } ->
      { Bonsai_tui_bar_chart.Bar.label = Some name; value = cpu; color = None })
  in
  let height =
    let%arr { height; width = _ } = dimensions in
    height * 3 / 4
  in
  let%arr height and bars in
  Bonsai_tui_bar_chart.view
    (module struct
      include Percent

      let to_float = to_percentage
      let of_float = of_percentage
    end)
    ~title:(Some "CPU % Usage by Process")
    ~max_bar_height:height
    ~y_max:(Constant (Percent.of_mult 2.0))
    ~bar_width
    ~bar_padding
    bars
;;

let app ~dimensions (local_ graph) =
  let view = make_view ~dimensions graph in
  let handler = Bonsai.return (fun _ -> Effect.Ignore) in
  ~view, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:{|Show cpu usage by process|}
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;

let () = Command_unix.run command
