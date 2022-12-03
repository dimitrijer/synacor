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

let of_bytes (bs : bytes) =
  { pc = A.low; mem = load_mem bs M.empty A.low; reg = R.empty; stack = S.empty }
;;

let of_ints (is : int list) =
  { pc = A.low
  ; mem =
      fst
      @@ List.fold_left
           (fun (mem, a) i -> M.write a (D.of_int i) mem, A.incr a)
           (M.empty, A.low)
           is
  ; reg = R.empty
  ; stack = S.empty
  }
;;

let run state = snd @@ Op.run_until_halt () state
