open Arch
open Rw

type spec =
  { name : string
  ; opcode : int
  ; exec : unit -> (Arch.t, bool) State.t
  }

let specs =
  [ (* [halt] *)
    { name = "halt"; opcode = 0; exec = State.(fun _ -> return true) }
  ; (* [set reg (reg|lit)] *)
    { name = "set"
    ; opcode = 1
    ; exec =
        State.(
          fun () ->
            let* r = fetch_reg () in
            let* v = load_reg_or_lit () in
            let* _ = write_reg r v in
            return false)
    }
  ; (* [push (reg|lit)] *)
    { name = "push"
    ; opcode = 2
    ; exec =
        State.(
          fun () ->
            let* op = load_reg_or_lit () in
            let* _ = push op in
            return false)
    }
  ; (* [pop (reg|mem)] *)
    { name = "pop"
    ; opcode = 3
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* v = pop () in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [eq (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "eq"
    ; opcode = 4
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let v = D.of_int @@ if D.eq op1 op2 then 1 else 0 in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [gt (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "gt"
    ; opcode = 5
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let v = D.of_int @@ if D.gt op1 op2 then 1 else 0 in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [jmp mem *)
    { name = "jmp"
    ; opcode = 6
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_addr () in
            let* state = get () in
            let* _ = put { state with pc = dst } in
            return false)
    }
  ; (* [jt (reg|lit) mem] *)
    { name = "jt"
    ; opcode = 7
    ; exec =
        State.(
          fun () ->
            let* op = load_reg_or_lit () in
            let* dst = fetch_addr () in
            if not (D.eq op (D.of_int 0))
            then
              let* state = get () in
              let* _ = put { state with pc = dst } in
              return false
            else return false)
    }
  ; (* [jf (reg|lit) mem] *)
    { name = "jf"
    ; opcode = 8
    ; exec =
        State.(
          fun () ->
            let* op = load_reg_or_lit () in
            let* dst = fetch_addr () in
            if D.eq op (D.of_int 0)
            then
              let* state = get () in
              let* _ = put { state with pc = dst } in
              return false
            else return false)
    }
  ; (* [add (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "add"
    ; opcode = 9
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let v = D.add op1 op2 in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [mult (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "mult"
    ; opcode = 10
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let v = D.mult op1 op2 in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [mod (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "mod"
    ; opcode = 11
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let v = D.modu op1 op2 in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [and (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "and"
    ; opcode = 12
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let v = D.logand op1 op2 in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [or (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "or"
    ; opcode = 13
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let v = D.logor op1 op2 in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [not (reg|mem) (reg|lit)] *)
    { name = "not"
    ; opcode = 14
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op = load_reg_or_lit () in
            let v = D.lognot op in
            let* _ = write_reg_or_addr dst v in
            return false)
    }
  ; (* [rmem (reg|mem) (reg|mem)] *)
    { name = "rmem"
    ; opcode = 15
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* src = fetch_reg_or_addr () in
            match src with
            | Either.Left r ->
              (* indirect addressing via register *)
              let* a = read_reg r in
              let* v = read_mem (a |> D.to_int |> A.of_int) in
              let* _ = write_reg_or_addr dst v in
              return false
            | Either.Right a ->
              let* v = read_mem a in
              let* _ = write_reg_or_addr dst v in
              return false)
    }
  ; (* [wmem (reg|mem) (reg|lit)] *)
    { name = "wmem"
    ; opcode = 16
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* v = load_reg_or_lit () in
            match dst with
            | Either.Left r ->
              (* indirect addressing via register *)
              let* a = read_reg r in
              let* _ = write_mem (a |> D.to_int |> A.of_int) v in
              return false
            | Either.Right a ->
              let* _ = write_mem a v in
              return false)
    }
  ; (* [call (reg|mem)] *)
    { name = "call"
    ; opcode = 17
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* state = get () in
            let* _ = push (state.pc |> A.to_int |> D.of_int) in
            (* stack state changed, so we do another get *)
            let* state' = get () in
            let* _ =
              match dst with
              | Either.Left r ->
                (* indirect addressing via register *)
                put { state' with pc = R.read r state'.reg |> D.to_int |> A.of_int }
              | Either.Right a -> put { state' with pc = a }
            in
            return false)
    }
  ; (* [ret] *)
    { name = "ret"
    ; opcode = 18
    ; exec =
        State.(
          fun () ->
            let* state = get () in
            if S.is_empty state.stack
            then return true
            else
              let* dst = pop () in
              (* have to get state again, we popped *)
              let* state' = get () in
              let* _ = put { state' with pc = dst |> D.to_int |> A.of_int } in
              return false)
    }
  ; (* [out (reg|lit)] *)
    { name = "out"
    ; opcode = 19
    ; exec =
        State.(
          fun () ->
            let* op = load_reg_or_lit () in
            print_char (op |> D.to_int |> Char.chr);
            return false)
    }
  ; (* [noop] *)
    { name = "noop"; opcode = 21; exec = State.(fun () -> return false) }
  ]
;;

let specs_by_opcode = List.map (fun spec -> spec.opcode, spec) specs

let decode ?(trace = false) () =
  State.(
    let* { pc; _ } = get () in
    let* opcode = fetch_data () in
    let spec_opt =
      Base.List.Assoc.find specs_by_opcode ~equal:Int.equal (D.to_int opcode)
    in
    match spec_opt with
    | Some spec ->
      if trace
      then Printf.fprintf Out_channel.stderr "[decode PC=%d] %s\n" (A.to_int pc) spec.name;
      return spec
    | None ->
      failwith
      @@ Printf.sprintf
           "[decode PC=%d] no such opcode: %d"
           (A.to_int pc)
           (Arch.D.to_int opcode))
;;

(* wrapper around exec to handle exceptions *)
let exec_w spec state =
  try spec.exec () state with
  | Failure s ->
    failwith
    @@ Printf.sprintf
         "[exec PC=%d] %s: unhandled exception: %s"
         (A.to_int state.pc)
         spec.name
         s
  | e ->
    failwith
    @@ Printf.sprintf
         "[exec PC=%d] %s: unhandled exception: %s"
         (A.to_int state.pc)
         spec.name
         (Printexc.to_string e)
;;

let rec run_until_halt ?(trace = false) () =
  State.(
    let* spec = decode ~trace () in
    let* is_halt = exec_w spec in
    if is_halt then return () else run_until_halt ())
;;
