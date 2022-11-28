module M = Mem.Make (Data.A) (Data.D)
module R = Reg.Make (Data.A) (Data.D)
module S = Stack

type state =
  { pc : Data.A.t
  ; mem : M.t
  ; stack : Data.D.t Stack.t
  ; reg : R.t
  }

type 'a t = state -> 'a * state

(** Monadic bind. *)
let bind (t : 'a t) (f : 'a -> 'b t) : 'b t =
 fun state ->
  let a, s' = t state in
  let b, s'' = f a s' in
  b, s''
;;

(** Monadic return. This is the same as [return a = fun state -> a, state] *)
let return (a : 'a) (state : state) = a, state

(** Applicative product. *)
let product (t1 : 'a t) (t2 : 'b t) : ('a * 'b) t =
 fun state ->
  let a, s' = t1 state in
  let b, s'' = t2 s' in
  (a, b), s''
;;

(** Applicative map. *)
let map ~(f : 'a -> 'b) (t : 'a t) : 'b t =
 fun state ->
  let a, s' = t state in
  f a, s'
;;

(** Monadic let operator. *)
let ( let* ) = bind

(** Applicative let operator. *)
let ( let+ ) x f = map f x

(** Applicative and operator. *)
let ( and+ ) = product

(** [get] returns a state as result. *)
let get () state = state, state

(** [put] replaces state. *)
let put s _ = (), s

(** [fetchw] fetches next word. *)
let fetchw () state = M.read state.pc state.mem, { state with pc = Data.A.incr state.pc }

(** [fetcha] fetches next word and interprets it as address. *)
let fetcha () =
  let* w = fetchw () in
  return @@ Data.A.of_int (Data.D.to_int w)
;;

(* Reads word at provided address. *)
let readw a state =
  match Data.A.to_reg_opt a with
  | Some r -> R.read r state.reg, state
  | None -> M.read a state.mem, state
;;

(* Writes word [v] at address [a]. *)
let writew a v =
  let* s = get () in
  match Data.A.to_reg_opt a with
  (* No return with put! *)
  | Some r -> put { s with reg = R.write r v s.reg }
  | None -> put { s with mem = M.write a v s.mem }
;;

let read_instr () =
  let* w1 = fetchw () in
  match Data.D.to_int w1 with
  | 0 -> return Op.Halt
  | 1 ->
    let* a = fetcha () in
    let* v = fetchw () in
    return @@ Op.Set (a, v)
  | 19 ->
    let* a = fetcha () in
    return @@ Op.Out a
  | 21 -> return Op.Noop
  | opcode -> failwith (Printf.sprintf "unknown opcode: %d" opcode)
;;

let rec exec () =
  let* op = read_instr () in
  (* let* s = get () in *)
  (* Printf.printf *)
  (*   "pc: %d regs: %s instr: %s\n" *)
  (*   (Data.A.to_int s.pc) *)
  (*   (R.to_string s.reg) *)
  (*   (Op.to_string op); *)
  match op with
  | Op.Halt -> return ()
  | Op.Set (a, bv) ->
    (* a has to point to register *)
    let (_ : Data.A.reg) = Data.A.to_reg a in
    let* _ = writew a bv in
    exec ()
  | Op.Noop -> exec ()
  | Op.Out a ->
    let* v = readw a in
    v |> Data.D.to_int |> Char.chr |> print_char;
    exec ()
;;

let run state = exec () state

let rec load_mem (bs : bytes) (mem : M.t) (a : Data.A.t) =
  match Bytes.length bs with
  | 0 -> mem
  | 1 -> failwith "leftover bytes after loading mem"
  | _ ->
    let v = Data.D.of_int (Bytes.get_uint16_le bs 0) in
    let mem' = M.write a v mem in
    let start = min 2 (Bytes.length bs - 1) in
    let bs' = Bytes.sub bs start @@ max 0 (Bytes.length bs - 2) in
    load_mem bs' mem' (Data.A.incr a)
;;

let of_bytes (bs : bytes) =
  { pc = Data.A.low
  ; mem = load_mem bs M.empty Data.A.low
  ; reg = R.empty
  ; stack = S.empty
  }
;;
