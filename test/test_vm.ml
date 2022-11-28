open Synacor

let write_op b op = List.iter (fun d -> Buffer.add_uint16_le b d) (Op.to_ints op)

let bytecode (ops : Op.t list) =
  let b = Buffer.create 128 in
  List.iter (write_op b) ops;
  Buffer.to_bytes b
;;

let run_vm ops =
  try ignore @@ Vm.run (Vm.of_bytes (bytecode ops)) with
  | e -> Printf.printf "unhandled exn: %s" (Printexc.to_string e)
;;

let%expect_test "uninitialized" =
  run_vm [];
  [%expect {|unhandled exn: (Failure "unknown opcode: 65535")|}]
;;

let%expect_test "no halt" =
  run_vm [ Op.Noop ];
  [%expect {|unhandled exn: (Failure "unknown opcode: 65535")|}]
;;

let%expect_test "noop" =
  run_vm [ Op.Noop; Op.Halt ];
  [%expect {||}]
;;

let%expect_test "multiple noops" =
  run_vm [ Op.Noop; Op.Noop; Op.Noop; Op.Halt ];
  [%expect {||}]
;;

let%expect_test "out" =
  run_vm
    [ Op.Set (Data.A.of_reg Data.A.R4, Data.D.of_int 65)
    ; Op.Out (Data.A.of_reg Data.A.R4)
    ; Op.Halt
    ];
  [%expect {|A|}]
;;
