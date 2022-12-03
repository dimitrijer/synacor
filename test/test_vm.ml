open Synacor

type op =
  | Noop
  | Set of (Data.R.t * Data.D.t)
  | Add of (Data.D.t * Data.D.t * Data.D.t)
  | Out of Data.D.t
  | Halt

let to_ints op =
  match op with
  | Halt -> [ 0 ]
  | Set (a, b) -> [ 1; Data.R.to_int a; Data.D.to_int b ]
  | Add (a, b, c) -> [ 9; Data.D.to_int a; Data.D.to_int b; Data.D.to_int c ]
  | Out a -> [ 19; Data.D.to_int a ]
  | Noop -> [ 21 ]
;;

let write_op b op = List.iter (fun d -> Buffer.add_uint16_le b d) (to_ints op)

let bytecode (ops : op list) =
  let b = Buffer.create 128 in
  List.iter (write_op b) ops;
  Buffer.to_bytes b
;;

let run_vm_ints (is : int list) =
  try ignore @@ Op.run (State.of_ints is) with
  | e -> Printf.printf "unhandled exn: %s" (Printexc.to_string e)
;;

let run_vm ops =
  try ignore @@ Op.run (State.of_bytes (bytecode ops)) with
  | e -> Printf.printf "unhandled exn: %s" (Printexc.to_string e)
;;

let%expect_test "uninitialized" =
  run_vm [];
  [%expect {|unhandled exn: (Not_found_s "List.Assoc.find_exn: not found")|}]
;;

let%expect_test "no halt" =
  run_vm [ Noop ];
  [%expect {|unhandled exn: (Not_found_s "List.Assoc.find_exn: not found")|}]
;;

let%expect_test "noop" =
  run_vm [ Noop; Halt ];
  [%expect {||}]
;;

let%expect_test "multiple noops" =
  run_vm [ Noop; Noop; Noop; Halt ];
  [%expect {||}]
;;

let%expect_test "out" =
  run_vm
    [ Set (Data.R4, Data.D.of_int 65)
    ; Out (Data.R4 |> Data.R.to_int |> Data.D.of_int)
    ; Halt
    ];
  [%expect {|A|}]
;;

(* let%expect_test "from spec" = *)
(*   run_vm_ints [ 9; 32768; 32769; 4; 19; 32768; 0 ]; *)
(*   [%expect {|A|}] *)
(* ;; *)
