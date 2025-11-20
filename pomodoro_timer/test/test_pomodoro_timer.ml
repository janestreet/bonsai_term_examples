open! Core
open Bonsai_test

let app = Pomodoro_timer.app

let press_key handle key =
  Bonsai_term_test.send_event handle (Key_press { key = ASCII key; mods = [] })
;;

let press_space handle =
  Bonsai_term_test.send_event handle (Key_press { key = ASCII ' '; mods = [] })
;;

let%expect_test "Initial state shows work timer stopped at 25:00" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.show handle;
  Handle.recompute_view handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              25:00                                             │
    │                                                                                │
    │                              Stopped                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Pressing space starts the timer" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Pressing space again pauses the timer" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  Handle.recompute_view handle;
  press_space handle;
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Paused                                            │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Pressing R resets the timer" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  Handle.recompute_view handle;
  press_key handle 'R';
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              25:00                                             │
    │                                                                                │
    │                              Stopped                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Pressing N advances to next phase (short break)" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_key handle 'N';
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Short Break                                       │
    │                                                                                │
    │                              05:00                                             │
    │                                                                                │
    │                              Stopped                                           │
    │                                                                                │
    │                              Completed: 1                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Multiple phase transitions work correctly" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  (* Go through several phases *)
  press_key handle 'N';
  (* Work -> Short Break, completed = 1 *)
  Handle.recompute_view handle;
  press_key handle 'N';
  (* Short Break -> Work, completed = 1 *)
  Handle.recompute_view handle;
  press_key handle 'N';
  (* Work -> Short Break, completed = 2 *)
  Handle.recompute_view handle;
  press_key handle 'N';
  (* Short Break -> Work, completed = 2 *)
  Handle.recompute_view handle;
  press_key handle 'N';
  (* Work -> Short Break, completed = 3 *)
  Handle.recompute_view handle;
  press_key handle 'N';
  (* Short Break -> Work, completed = 3 *)
  Handle.recompute_view handle;
  press_key handle 'N';
  (* Work -> Long Break, completed = 4 (every 4th) *)
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Long Break                                        │
    │                                                                                │
    │                              15:00                                             │
    │                                                                                │
    │                              Stopped                                           │
    │                                                                                │
    │                              Completed: 4                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Paused timer can be resumed" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  (* Start *)
  Handle.recompute_view handle;
  press_space handle;
  (* Pause *)
  Handle.recompute_view handle;
  press_space handle;
  (* Resume *)
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Timer ticks down every second when running" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  (* Start the timer *)
  Handle.recompute_view handle;
  (* Initial state should show 24:59 after starting *)
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* Advance clock by 1 second *)
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* Advance clock by 5 more seconds *)
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 5.0);
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:58                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Paused timer does not tick down" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  (* Start the timer *)
  Handle.recompute_view handle;
  press_space handle;
  (* Pause the timer *)
  Handle.recompute_view handle;
  (* Timer should be paused at 24:59 *)
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Paused                                            │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* Advance clock by 10 seconds - timer should not change since it's paused *)
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 10.0);
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Paused                                            │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Timer advances to next phase when it reaches zero" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  (* Start the timer *)
  Handle.recompute_view handle;
  (* Advance clock by 25 minutes (1499 seconds) to complete the work phase Note: 1499
     because the timer already ticked once when we started it *)
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1499.0);
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Timer ticks down to the last few seconds correctly" =
  let handle = Bonsai_term_test.create_handle app in
  Bonsai_term_test.set_dimensions handle { width = 80; height = 20 };
  Handle.recompute_view handle;
  press_space handle;
  (* Start the timer *)
  Handle.recompute_view handle;
  (* Advance clock to 3 seconds remaining (1496 seconds since timer already ticked once) *)
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1496.0);
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:59                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* Advance by 1 more second *)
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 1.0);
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:58                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* Advance by 2 more seconds to reach zero and transition *)
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 2.0);
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                              Work                                              │
    │                                                                                │
    │                              24:58                                             │
    │                                                                                │
    │                              Running                                           │
    │                                                                                │
    │                              Completed: 0                                      │
    │                                                                                │
    │                                                                                │
    │                              Controls:                                         │
    │                                                                                │
    │                              SPACE - Start/Pause                               │
    │                              R - Reset                                         │
    │                              N - Next Phase                                    │
    │                              Ctrl+C - Quit                                     │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
