open Arch

let read_mem a state = M.read a state.mem, state
let read_reg r state = R.read r state.reg, state
let write_reg r v state = (), { state with reg = R.write r v state.reg }
let write_mem a v state = (), { state with mem = M.write a v state.mem }

let fetch_data () =
  State.(
    let* s = get () in
    let* d = read_mem s.pc in
    let* _ = put { s with pc = A.incr s.pc } in
    return d)
;;

let fetch_reg () =
  State.(
    let* d = fetch_data () in
    match RI.of_int_opt (D.to_int d) with
    | Some r -> return r
    | None -> failwith @@ Printf.sprintf "not a register address: %d" (D.to_int d))
;;

let fetch_lit () =
  State.(
    let* d = fetch_data () in
    (* Literals have same range as addresses. *)
    match A.of_int_opt (D.to_int d) with
    | Some _ -> return d
    | None -> failwith @@ Printf.sprintf "not a literal: %d" (D.to_int d))
;;

let fetch_reg_or_addr () =
  State.(
    let* d = fetch_data () in
    match RI.of_int_opt (D.to_int d) with
    | Some r -> return @@ Either.left r
    | None -> return @@ Either.right (d |> D.to_int |> A.of_int))
;;

let load_reg_or_lit () =
  State.(
    let* d = fetch_data () in
    match d |> D.to_int |> RI.of_int_opt with
    | Some r -> read_reg r
    | None -> return d)
;;
