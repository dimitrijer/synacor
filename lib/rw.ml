open Arch

(* The following are useful state transitions for reading/writing to memory,
   stack and registers. *)
let read_mem a state = M.read a state.mem, state
let read_reg r state = R.read r state.reg, state
let write_reg r v state = (), { state with reg = R.write r v state.reg }
let write_mem a v state = (), { state with mem = M.write a v state.mem }
let push v state = (), { state with stack = S.push v state.stack }

let pop () state =
  let v, stack' = S.pop state.stack in
  v, { state with stack = stack' }
;;

(** [fetch_data ()] is a state transition that fetches next word from memory
    pointed to by PC value. PC value is also incremented. *)
let fetch_data () =
  State.(
    let* s = get () in
    let* d = read_mem s.pc in
    let* _ = put { s with pc = A.incr s.pc } in
    return d)
;;

(** [fetch_reg ()] is a state transition that fetches next word from memory
    pointed to by PC value, and interprets is as register identifier. PC value
    is also incremented. *)
let fetch_reg () =
  State.(
    let* d = fetch_data () in
    match RI.of_int_opt (D.to_int d) with
    | Some r -> return r
    | None -> failwith @@ Printf.sprintf "not a register address: %d" (D.to_int d))
;;

(** [fetch_addr ()] is a state transition that fetches next word from memory
    pointed to by PC value, and interprets is as a memory address. PC value
    is also incremented. *)
let fetch_addr () =
  State.(
    let* d = fetch_data () in
    match RI.of_int_opt (D.to_int d) with
    | Some _ -> failwith @@ Printf.sprintf "not a memory address: %d" (D.to_int d)
    | None -> return (d |> D.to_int |> A.of_int))
;;

(** [fetch_reg_or_addr ()] acts as a combination of [fetch_reg ()] and
    [fetch_addr ()] -- it returns an [Either] representing a memory address or
    a register identifier. PC value is also incremented. *)
let fetch_reg_or_addr () =
  State.(
    let* d = fetch_data () in
    match RI.of_int_opt (D.to_int d) with
    | Some r -> return @@ Either.left r
    | None -> return @@ Either.right (d |> D.to_int |> A.of_int))
;;

(** [load_reg_or_lit ()] is a state transition that fetches next word from
    memory pointed to by PC value. If it's a register identifier, this function
    returns the value in the register. Otherwise, fetched word is
    interpreted as a literal. *)
let load_reg_or_lit () =
  State.(
    let* d = fetch_data () in
    match d |> D.to_int |> RI.of_int_opt with
    | Some r -> read_reg r
    | None -> return d)
;;

(** [write_reg_or_addr a v] writes value [v] to either register identified by
    [a], or memory location [a]. *)
let write_reg_or_addr a v =
  match a with
  | Either.Left r -> write_reg r v
  | Either.Right a -> write_mem a v
;;

(** [read_next_char_stdin] reads next character from input buffer. If input
    buffer is empty, this function will read next line from stdin, up until
    newline character. *)
let rec read_next_char_stdin () =
  State.(
    let* state = get () in
    if String.length state.inbuf > 0
    then (
      let rem = String.sub state.inbuf 1 (String.length state.inbuf - 1) in
      let* _ = put { state with inbuf = rem } in
      return @@ String.get state.inbuf 0)
    else (
      let line = read_line () in
      let* _ = put { state with inbuf = line } in
      read_next_char_stdin ()))
;;
