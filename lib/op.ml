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
  ; (* [set reg lit] *)
    { name = "set"
    ; opcode = 1
    ; exec =
        State.(
          fun () ->
            let* r = fetch_reg () in
            let* lit = fetch_lit () in
            let* _ = write_reg r lit in
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
            let* v = pop in
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
            let result = D.of_int @@ if D.eq op1 op2 then 1 else 0 in
            let* _ = write_reg_or_addr dst result in
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
            let result = D.of_int @@ if D.gt op1 op2 then 1 else 0 in
            let* _ = write_reg_or_addr dst result in
            return false)
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
            let result = D.add op1 op2 in
            let* _ = write_reg_or_addr dst result in
            return false)
    }
  ; (* [rmem (reg|mem) mem] *)
    { name = "rmem"
    ; opcode = 15
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* src = fetch_addr () in
            let* srcv = read_mem src in
            let* _ = write_reg_or_addr dst srcv in
            return false)
    }
  ; (* [wmem (reg|mem) mem] *)
    { name = "wmem"
    ; opcode = 16
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_addr () in
            let* srcv = load_reg_or_lit () in
            let* _ = write_mem dst srcv in
            return false)
    }
  ; (* [out (reg|mem)] *)
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

let decode () =
  State.(
    let* opcode = fetch_data () in
    let spec_opt =
      Base.List.Assoc.find specs_by_opcode ~equal:Int.equal (D.to_int opcode)
    in
    match spec_opt with
    | Some spec -> return spec
    | None -> failwith @@ Printf.sprintf "no such opcode: %d" (Arch.D.to_int opcode))
;;

let rec run_until_halt () =
  State.(
    let* spec = decode () in
    let* is_halt = spec.exec () in
    if is_halt then return () else run_until_halt ())
;;
