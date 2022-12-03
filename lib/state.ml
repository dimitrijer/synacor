module M = Mem.Make (Data.A) (Data.D)
module R = Reg.Make (Data.R) (Data.D)

type state =
  { pc : Data.A.t
  ; mem : M.t
  ; stack : Data.D.t Stack.t
  ; reg : R.t
  }

type 'a t = state -> 'a * state

let bind ma f state =
  let a, s' = ma state in
  let b, s'' = f a s' in
  b, s''
;;

let return a state = a, state

let product ma mb state =
  let a, s' = ma state in
  let b, s'' = mb s' in
  (a, b), s''
;;

let map f ma state =
  let a, s' = ma state in
  f a, s'
;;

let ( let* ) = bind
let ( let+ ) = map
let ( and+ ) = product
let get () state = state, state
let put s _ = (), s

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
  ; stack = Stack.empty
  }
;;

let of_ints (is : int list) =
  { pc = Data.A.low
  ; mem =
      fst
      @@ List.fold_left
           (fun (mem, a) i -> M.write a (Data.D.of_int i) mem, Data.A.incr a)
           (M.empty, Data.A.low)
           is
  ; reg = R.empty
  ; stack = Stack.empty
  }
;;
