open Arch

let read_mem a state = M.read a state.mem, state
let read_reg r state = R.read r state.reg, state
let write_reg r v state = (), { state with reg = R.write r v state.reg }
let write_mem a v state = (), { state with mem = M.write a v state.mem }
let push v state = (), { state with stack = S.push v state.stack }

let pop () state =
  let v, stack' = S.pop state.stack in
  v, { state with stack = stack' }
;;

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

let fetch_addr () =
  State.(
    let* d = fetch_data () in
    match RI.of_int_opt (D.to_int d) with
    | Some _ -> failwith @@ Printf.sprintf "not a memory address: %d" (D.to_int d)
    | None -> return (d |> D.to_int |> A.of_int))
;;

let load_reg_or_lit () =
  State.(
    let* d = fetch_data () in
    match d |> D.to_int |> RI.of_int_opt with
    | Some r -> read_reg r
    | None -> return d)
;;

let fetch_reg_or_addr () =
  State.(
    let* d = fetch_data () in
    match RI.of_int_opt (D.to_int d) with
    | Some r -> return @@ Either.left r
    | None -> return @@ Either.right (d |> D.to_int |> A.of_int))
;;

let write_reg_or_addr a v =
  match a with
  | Either.Left r -> write_reg r v
  | Either.Right a -> write_mem a v
;;

let rec next_char () =
  State.(
    let* state = get () in
    if String.length state.inbuf > 0
    then
      let* _ =
        put
          { state with inbuf = String.sub state.inbuf 1 (String.length state.inbuf - 1) }
      in
      return @@ String.get state.inbuf 0
    else (
      let line = read_line () in
      let* _ = put { state with inbuf = line } in
      next_char ()))
;;
