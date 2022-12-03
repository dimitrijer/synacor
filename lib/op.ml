type spec =
  { name : string
  ; opcode : int
  ; exec : unit -> bool State.t
  }

let read_mem a state = State.(M.read a state.mem), state
let read_reg r state = State.(R.read r state.reg), state
let write_reg r v state = State.((), { state with reg = State.(R.write r v state.reg) })
let write_mem a v state = State.((), { state with mem = State.(M.write a v state.mem) })

let fetch_data () =
  State.(
    let* s = get () in
    let* d = read_mem s.pc in
    let* _ = put { s with pc = Data.A.incr s.pc } in
    return d)
;;

let fetch_reg () =
  State.(
    let* d = fetch_data () in
    match Data.R.of_int_opt (Data.D.to_int d) with
    | Some r -> return r
    | None -> failwith @@ Printf.sprintf "not a register address: %d" (Data.D.to_int d))
;;

let fetch_lit () =
  State.(
    let* d = fetch_data () in
    (* Literals have same range as addresses. *)
    match Data.A.of_int_opt (Data.D.to_int d) with
    | Some _ -> return d
    | None -> failwith @@ Printf.sprintf "not a literal: %d" (Data.D.to_int d))
;;

let fetch_reg_or_addr () =
  State.(
    let* d = fetch_data () in
    match Data.R.of_int_opt (Data.D.to_int d) with
    | Some r -> return @@ Either.left r
    | None -> return @@ Either.right (d |> Data.D.to_int |> Data.A.of_int))
;;

let load_reg_or_lit () =
  State.(
    let* d = fetch_data () in
    match d |> Data.D.to_int |> Data.R.of_int_opt with
    | Some r -> read_reg r
    | None -> return d)
;;

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
  ; (* [add (reg|mem) (reg|lit) (reg|lit)] *)
    { name = "add"
    ; opcode = 9
    ; exec =
        State.(
          fun () ->
            let* dst = fetch_reg_or_addr () in
            let* op1 = load_reg_or_lit () in
            let* op2 = load_reg_or_lit () in
            let result = Data.D.add op1 op2 in
            let* _ =
              match dst with
              | Either.Left r -> write_reg r result
              | Either.Right a -> write_mem a result
            in
            return false)
    }
  ; (* [out (reg|mem)] *)
    { name = "out"
    ; opcode = 19
    ; exec =
        State.(
          fun () ->
            let* op = load_reg_or_lit () in
            print_char (op |> Data.D.to_int |> Char.chr);
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
    return
    @@ Base.List.Assoc.find_exn specs_by_opcode ~equal:Int.equal (Data.D.to_int opcode))
;;

let rec run_until_halt () =
  State.(
    let* spec = decode () in
    let* is_halt = spec.exec () in
    if is_halt then return () else run_until_halt ())
;;

let run state = snd @@ run_until_halt () state
