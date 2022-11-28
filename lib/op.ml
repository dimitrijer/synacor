type t =
  | Halt
  | Set of Data.A.t * Data.D.t
  | Out of Data.A.t
  | Noop

let to_string = function
  | Halt -> "halt"
  | Set (a, bv) -> Printf.sprintf "set %s %d" (Data.A.to_string a) (Data.D.to_int bv)
  | Out a -> Printf.sprintf "out %s" (Data.A.to_string a)
  | Noop -> "noop"
;;

let code = function
  | Halt -> 0
  | Set _ -> 1
  | Out _ -> 19
  | Noop -> 21
;;

let to_ints op =
  code op
  ::
  (match op with
  | Halt -> []
  | Set (a, bv) -> [ Data.A.to_int a; Data.D.to_int bv ]
  | Out a -> [ Data.A.to_int a ]
  | Noop -> [])
;;
