open Synacor
open Arch

type op =
  | Noop
  | Set of (RI.t * D.t)
  | Push of D.t
  | Pop of D.t
  | Eq of (D.t * D.t * D.t)
  | Gt of (D.t * D.t * D.t)
  | Jmp of A.t
  | Add of (D.t * D.t * D.t)
  | Rmem of (D.t * A.t)
  | Wmem of (A.t * D.t)
  | Out of D.t
  | Halt

let to_ints op =
  match op with
  | Halt -> [ 0 ]
  | Set (a, b) -> [ 1; RI.to_int a; D.to_int b ]
  | Push a -> [ 2; D.to_int a ]
  | Pop a -> [ 3; D.to_int a ]
  | Eq (a, b, c) -> [ 4; D.to_int a; D.to_int b; D.to_int c ]
  | Gt (a, b, c) -> [ 5; D.to_int a; D.to_int b; D.to_int c ]
  | Jmp a -> [ 6; A.to_int a ]
  | Add (a, b, c) -> [ 9; D.to_int a; D.to_int b; D.to_int c ]
  | Rmem (a, b) -> [ 15; D.to_int a; A.to_int b ]
  | Wmem (a, b) -> [ 16; A.to_int a; D.to_int b ]
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

let%expect_test "rmem and wmem" =
  run_vm
    [ Set (R0, D.of_int 50)
    ; Wmem (A.of_int 12345, R0 |> RI.to_int |> D.of_int)
    ; Rmem (R1 |> RI.to_int |> D.of_int, A.of_int 12345)
    ; Out (R1 |> RI.to_int |> D.of_int)
    ; (* ascii 50 = '2' *)
      Halt
    ];
  [%expect {|2|}]
;;

let%expect_test "eq" =
  run_vm
    [ Set (R0, D.of_int 51)
    ; Eq (R1 |> RI.to_int |> D.of_int, R0 |> RI.to_int |> D.of_int, D.of_int 49)
    ; Eq (R2 |> RI.to_int |> D.of_int, R0 |> RI.to_int |> D.of_int, D.of_int 51)
    ; Add (R3 |> RI.to_int |> D.of_int, R1 |> RI.to_int |> D.of_int, D.of_int 50)
      (* 50 + 0 = 50 (ascii 2) *)
    ; Add (R4 |> RI.to_int |> D.of_int, R2 |> RI.to_int |> D.of_int, D.of_int 50)
      (* 50 + 1 = 51 (ascii 3) *)
    ; Out (R3 |> RI.to_int |> D.of_int)
    ; Out (R4 |> RI.to_int |> D.of_int)
    ; Halt
    ];
  [%expect {|23|}]
;;

let%expect_test "gt" =
  run_vm
    [ Set (R0, D.of_int 123)
    ; Gt (R1 |> RI.to_int |> D.of_int, D.of_int 150, R0 |> RI.to_int |> D.of_int)
    ; Gt (R2 |> RI.to_int |> D.of_int, R0 |> RI.to_int |> D.of_int, D.of_int 200)
    ; Add (R3 |> RI.to_int |> D.of_int, R1 |> RI.to_int |> D.of_int, D.of_int 50)
      (* 50 + 1 = 51 (ascii 3) *)
    ; Add (R4 |> RI.to_int |> D.of_int, R2 |> RI.to_int |> D.of_int, D.of_int 50)
      (* 50 + 0 = 50 (ascii 2) *)
    ; Out (R3 |> RI.to_int |> D.of_int)
    ; Out (R4 |> RI.to_int |> D.of_int)
    ; Halt
    ];
  [%expect {|32|}]
;;

let%expect_test "gt" =
  run_vm
    [ Noop
    ; Jmp (A.of_int 5)
    ; Out (D.of_int 48) (* ascii 0, skipped over *)
    ; Out (D.of_int 49) (* ascii 1 *)
    ; Halt
    ];
  [%expect {|1|}]
;;
(* let%expect_test "from spec" = *)
(*   run_vm_ints [ 9; 32768; 32769; 4; 19; 32768; 0 ]; *)
(*   [%expect {|A|}] *)
(* ;; *)
