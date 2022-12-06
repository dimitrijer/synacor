open Synacor
open Arch

let to_d ri = ri |> RI.to_int |> D.of_int

type op =
  | Noop
  | Set of (RI.t * D.t)
  | Push of D.t
  | Pop of D.t
  | Eq of (D.t * D.t * D.t)
  | Gt of (D.t * D.t * D.t)
  | Jmp of A.t
  | Jt of (D.t * A.t)
  | Jf of (D.t * A.t)
  | Add of (D.t * D.t * D.t)
  | Mult of (D.t * D.t * D.t)
  | Mod of (D.t * D.t * D.t)
  | And of (D.t * D.t * D.t)
  | Or of (D.t * D.t * D.t)
  | Not of (D.t * D.t)
  | Rmem of (D.t * D.t)
  | Wmem of (D.t * D.t)
  | Call of D.t
  | Ret
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
  | Jt (a, b) -> [ 7; D.to_int a; A.to_int b ]
  | Jf (a, b) -> [ 8; D.to_int a; A.to_int b ]
  | Add (a, b, c) -> [ 9; D.to_int a; D.to_int b; D.to_int c ]
  | Mult (a, b, c) -> [ 10; D.to_int a; D.to_int b; D.to_int c ]
  | Mod (a, b, c) -> [ 11; D.to_int a; D.to_int b; D.to_int c ]
  | And (a, b, c) -> [ 12; D.to_int a; D.to_int b; D.to_int c ]
  | Or (a, b, c) -> [ 13; D.to_int a; D.to_int b; D.to_int c ]
  | Not (a, b) -> [ 14; D.to_int a; D.to_int b ]
  | Rmem (a, b) -> [ 15; D.to_int a; D.to_int b ]
  | Wmem (a, b) -> [ 16; D.to_int a; D.to_int b ]
  | Call a -> [ 17; D.to_int a ]
  | Ret -> [ 18 ]
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
  (* uninitialized mem contains 0, which is halt *)
  [%expect {||}]
;;

let%expect_test "no halt" =
  run_vm [ Noop ];
  (* uninitialized mem contains 0, which is halt *)
  [%expect {||}]
;;

let%expect_test "noop" =
  run_vm [ Noop; Halt ];
  [%expect {||}]
;;

let%expect_test "multiple noops" =
  run_vm [ Noop; Noop; Noop; Halt ];
  [%expect {||}]
;;

let%expect_test "set with register input" =
  run_vm [ Set (R3, D.of_int 66); Set (R4, to_d R3); Out (to_d R4); Halt ];
  [%expect {|B|}]
;;

let%expect_test "set and out" =
  run_vm [ Set (R4, D.of_int 65); Out (to_d R4); Halt ];
  [%expect {|A|}]
;;

let%expect_test "add" =
  run_vm
    [ Set (R6, D.of_int 32)
    ; Set (R2, D.of_int 10)
    ; Add (to_d R3, to_d R2, to_d R6)
    ; Out (to_d R3)
    ; Halt
    ];
  [%expect {|*|}]
;;

let%expect_test "add overflows" =
  run_vm
    [ Set (R6, D.of_int 32760)
    ; Set (R2, D.of_int 51)
    ; Add (to_d R3, to_d R2, to_d R6)
    ; Out (to_d R3) (* ascii 43 = + *)
    ; Halt
    ];
  [%expect {|+|}]
;;

let%expect_test "mult" =
  run_vm
    [ Set (R3, D.of_int 11); Mult (to_d R5, D.of_int 11, to_d R3); Out (to_d R5); Halt ];
  (* ascii 121 = y *)
  [%expect {|y|}]
;;

let%expect_test "mult overflows" =
  run_vm
    [ Set (R3, D.of_int 293); Mult (to_d R5, D.of_int 112, to_d R3); Out (to_d R5); Halt ];
  (* ascii 48 = 0 *)
  [%expect {|0|}]
;;

let%expect_test "mod" =
  run_vm
    [ Mod (to_d R7, D.of_int 502, D.of_int 67); Out (to_d R7) (* ascii 33 = ! *); Halt ];
  [%expect {|!|}]
;;

let%expect_test "and" =
  run_vm
    [ And (to_d R7, D.of_int 0xF0, D.of_int 0x28)
    ; Out (to_d R7) (* ascii 32 = <Space> *)
    ; Halt
    ];
  [%expect {| |}]
;;

let%expect_test "or" =
  run_vm
    [ Or (to_d R4, D.of_int 0x40, D.of_int 0x03); Out (to_d R4) (* ascii 67 = C *); Halt ];
  [%expect {|C|}]
;;

let%expect_test "not" =
  run_vm
    [ Not (to_d R5, D.of_int 0x7FB5)
    ; And (to_d R5, to_d R5, D.of_int 0xFF)
    ; Out (to_d R5) (* ascii 4A = J *)
    ; Halt
    ];
  [%expect {|J|}]
;;

let%expect_test "push and pop" =
  run_vm
    [ Set (R1, D.of_int 55)
    ; Push (to_d R1)
    ; Set (R1, D.of_int 48)
    ; Push (to_d R1)
    ; Pop (to_d R2)
    ; Out (to_d R2) (* ascii 48 = '0' *)
    ; Out (to_d R2) (* ascii 48 = '0' *)
    ; Pop (to_d R3)
    ; Out (to_d R3) (* ascii 55 = '7' *)
    ; Halt
    ];
  [%expect {|007|}]
;;

let%expect_test "pop throws on empty stack" =
  run_vm [ Pop (to_d R3); Halt ];
  [%expect {|unhandled exn: Arch__Stack.Stack_Empty|}]
;;

let%expect_test "rmem and wmem" =
  run_vm
    [ Wmem (D.of_int 11, D.of_int 50)
    ; Rmem (to_d R1, D.of_int 11)
    ; Out (to_d R1) (* ascii 50 = '2' *)
    ; Halt
    ; Noop
    ; Noop
    ; Noop
    ; Noop
    ];
  [%expect {|2|}]
;;

let%expect_test "rmem and wmem indir" =
  run_vm
    [ Noop
    ; Noop
    ; Noop
    ; Set (R1, D.of_int 50) (* ascii 50 = 2 *)
    ; Noop
    ; Noop
    ; Noop
    ; Set (R0, D.of_int 5) (* pc 5 = operand to set above *)
    ; Rmem (to_d R1, to_d R0)
    ; Out (to_d R1)
    ; Wmem (to_d R0, D.of_int 57) (* write 57 to addr 5 *)
    ; Rmem (to_d R3, to_d R0)
    ; Out (to_d R3) (* ascii 57 = '9' *)
    ; Halt
    ];
  [%expect {|29|}]
;;

let%expect_test "eq" =
  run_vm
    [ Set (R0, D.of_int 51)
    ; Eq (to_d R1, to_d R0, D.of_int 49)
    ; Eq (to_d R2, to_d R0, D.of_int 51)
    ; Add (to_d R3, to_d R1, D.of_int 50) (* 50 + 0 = 50 (ascii 2) *)
    ; Add (to_d R4, to_d R2, D.of_int 50) (* 50 + 1 = 51 (ascii 3) *)
    ; Out (to_d R3)
    ; Out (to_d R4)
    ; Halt
    ];
  [%expect {|23|}]
;;

let%expect_test "gt" =
  run_vm
    [ Set (R0, D.of_int 123)
    ; Gt (to_d R1, D.of_int 150, to_d R0)
    ; Gt (to_d R2, to_d R0, D.of_int 200)
    ; Add (to_d R3, to_d R1, D.of_int 50) (* 50 + 1 = 51 (ascii 3) *)
    ; Add (to_d R4, to_d R2, D.of_int 50) (* 50 + 0 = 50 (ascii 2) *)
    ; Out (to_d R3)
    ; Out (to_d R4)
    ; Halt
    ];
  [%expect {|32|}]
;;

let%expect_test "jmp" =
  run_vm
    [ Noop
    ; Jmp (A.of_int 5)
    ; Out (D.of_int 48) (* ascii 0 *)
    ; Out (D.of_int 49) (* <-- (jmp) ascii 1 *)
    ; Halt
    ];
  [%expect {|1|}]
;;

let%expect_test "jt" =
  run_vm
    [ Noop
    ; Eq (to_d R3, D.of_int 1, D.of_int 2)
    ; Jt (to_d R3, A.of_int 10)
    ; Out (D.of_int 48) (* ascii 0 *)
    ; Eq (to_d R4, D.of_int 1, D.of_int 1)
    ; Jt (to_d R4, A.of_int 19)
    ; Out (D.of_int 49) (* ascii 1 *)
    ; Out (D.of_int 50) (* <-- (jt) ascii 2 *)
    ; Halt
    ];
  [%expect {|02|}]
;;

let%expect_test "jt jumps when != 1" =
  run_vm
    [ Noop
    ; Set (R3, D.of_int 0)
    ; Jt (to_d R3, A.of_int 9)
    ; Out (D.of_int 48) (* ascii 0 *)
    ; Set (R4, D.of_int 7777)
    ; Jt (to_d R4, A.of_int 17)
    ; Out (D.of_int 49) (* ascii 1 *)
    ; Out (D.of_int 50) (* <-- (jt) ascii 2 *)
    ; Halt
    ];
  [%expect {|02|}]
;;

let%expect_test "jf" =
  run_vm
    [ Noop
    ; Eq (R3 |> RI.to_int |> D.of_int, D.of_int 1, D.of_int 2)
    ; Jf (to_d R3, A.of_int 10)
    ; Out (D.of_int 48) (* ascii 0 *)
    ; Eq (R4 |> RI.to_int |> D.of_int, D.of_int 1, D.of_int 1) (* <-- (jf) *)
    ; Jf (to_d R4, A.of_int 19)
    ; Out (D.of_int 49) (* ascii 1 *)
    ; Out (D.of_int 50) (* ascii 2 *)
    ; Halt
    ];
  [%expect {|12|}]
;;

let%expect_test "call and ret" =
  run_vm
    [ Call (D.of_int 17)
    ; Call (D.of_int 23)
    ; Call (D.of_int 17)
    ; Out (D.of_int 48) (* ascii 0 *)
    ; Call (D.of_int 17)
    ; Call (D.of_int 17)
    ; Call (D.of_int 23)
    ; Halt
    ; Noop
    ; Noop
    ; Noop
    ; Out (D.of_int 49) (* ascii 1 *)
    ; Ret
    ; Noop
    ; Noop
    ; Noop
    ; Out (D.of_int 50) (* ascii 2 *)
    ; Ret
    ];
  [%expect {|1210112|}]
;;

let%expect_test "call and ret indirect" =
  run_vm
    [ Set (R0, D.of_int 22)
    ; Set (R1, D.of_int 28)
    ; Call (to_d R0)
    ; Call (to_d R1)
    ; Call (to_d R0)
    ; Out (D.of_int 48) (* ascii 0 *)
    ; Call (to_d R0)
    ; Call (to_d R0)
    ; Call (to_d R1)
    ; Halt
    ; Noop
    ; Noop
    ; Noop
    ; Out (D.of_int 49) (* ascii 1 *)
    ; Ret
    ; Noop
    ; Noop
    ; Noop
    ; Out (D.of_int 50) (* ascii 2 *)
    ; Ret
    ];
  [%expect {|1210112|}]
;;
