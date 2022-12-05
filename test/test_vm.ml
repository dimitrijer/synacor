open Synacor
open Arch

type op =
  | Noop
  | Set of (RI.t * D.t)
  | Add of (D.t * D.t * D.t)
  | Push of D.t
  | Pop of D.t
  | Out of D.t
  | Halt

let to_ints op =
  match op with
  | Halt -> [ 0 ]
  | Set (a, b) -> [ 1; RI.to_int a; D.to_int b ]
  | Add (a, b, c) -> [ 9; D.to_int a; D.to_int b; D.to_int c ]
  | Push a -> [ 2; D.to_int a ]
  | Pop a -> [ 3; D.to_int a ]
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
  [%expect {|unhandled exn: Failure("no such opcode: 65535")|}]
;;

let%expect_test "no halt" =
  run_vm [ Noop ];
  [%expect {|unhandled exn: Failure("no such opcode: 65535")|}]
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

let%expect_test "add" =
  run_vm
    [ Set (R6, D.of_int 32)
    ; Set (R2, D.of_int 10)
    ; Add
        ( R3 |> RI.to_int |> D.of_int
        , R2 |> RI.to_int |> D.of_int
        , R6 |> RI.to_int |> D.of_int )
    ; Out (R3 |> RI.to_int |> D.of_int)
    ; Halt
    ];
  [%expect {|*|}]
;;

let%expect_test "push and pop" =
  run_vm
    [ Set (R1, D.of_int 55)
    ; Push (R1 |> RI.to_int |> D.of_int)
    ; Set (R1, D.of_int 48)
    ; Push (R1 |> RI.to_int |> D.of_int)
    ; Pop (R2 |> RI.to_int |> D.of_int)
    ; Out (R2 |> RI.to_int |> D.of_int) (* ascii 48 = '0' *)
    ; Out (R2 |> RI.to_int |> D.of_int) (* ascii 48 = '0' *)
    ; Pop (R3 |> RI.to_int |> D.of_int)
    ; Out (R3 |> RI.to_int |> D.of_int) (* ascii 55 = '7' *)
    ; Halt
    ];
  [%expect {|007|}]
;;

let%expect_test "pop throws on empty stack" =
  run_vm [ Pop (R3 |> RI.to_int |> D.of_int); Halt ];
  [%expect {|unhandled exn: Failure("stack empty")|}]
;;
(* let%expect_test "from spec" = *)
(*   run_vm_ints [ 9; 32768; 32769; 4; 19; 32768; 0 ]; *)
(*   [%expect {|A|}] *)
(* ;; *)
