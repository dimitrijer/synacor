open Arch

let rec load_mem (bs : bytes) (mem : M.t) (a : A.t) =
  match Bytes.length bs with
  | 0 -> mem
  | 1 -> failwith "leftover bytes after loading mem"
  | _ ->
    let v = D.of_int (Bytes.get_uint16_le bs 0) in
    let mem' = M.write a v mem in
    let start = min 2 (Bytes.length bs - 1) in
    let bs' = Bytes.sub bs start @@ max 0 (Bytes.length bs - 2) in
    load_mem bs' mem' (A.incr a)
;;

let of_bytes ?(inbuf = String.empty) bs = create ~inbuf @@ load_mem bs M.empty A.low

let of_ints is =
  create
  @@ fst
  @@ List.fold_left
       (fun (mem, a) i -> M.write a (D.of_int i) mem, A.incr a)
       (M.empty, A.low)
       is
;;

(** [run s] runs VM at state [s]. *)
let run state = snd @@ Op.run_until_halt () state
