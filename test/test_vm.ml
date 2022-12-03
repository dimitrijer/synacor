open Synacor
open Arch

type op =
  | Noop
  | Set of (RI.t * D.t)
  | Add of (D.t * D.t * D.t)
  | Out of D.t
  | Halt

let to_ints op =
  match op with
  | Halt -> [ 0 ]
  | Set (a, b) -> [ 1; RI.to_int a; D.to_int b ]
  | Add (a, b, c) -> [ 9; D.to_int a; D.to_int b; D.to_int c ]
  | Out a -> [ 19; D.to_int a ]
  | Noop -> [ 21 ]
;;

let write_op b op = List.iter (fun d -> Buffer.add_uint16_le b d) (to_ints op)

let bytecode (ops : op list) =
  let b = Buffer.create 128 in
  List.iter (write_op b) ops;
  Buffer.to_bytes b
;;

let run_vm_ints (is : int list) =
  try ignore @@ Vm.run (Vm.of_ints is) with
  | e -> Printf.printf "unhandled exn: %s" (Printexc.to_string e)
;;

let run_vm ops =
  try ignore @@ Vm.run (Vm.of_bytes (bytecode ops)) with
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
  run_vm [ Set (R4, D.of_int 65); Out (R4 |> RI.to_int |> D.of_int); Halt ];
  [%expect {|A|}]
;;

(* let%expect_test "from spec" = *)
(*   run_vm_ints [ 9; 32768; 32769; 4; 19; 32768; 0 ]; *)
(*   [%expect {|A|}] *)
(* ;; *)
