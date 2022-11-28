module type REG = sig
  type t
  type r
  type data

  val write : r -> data -> t -> t
  val read : r -> t -> data
  val to_string : t -> string
  val empty : t
end

module Make (A : Data.ADDR) (D : Data.DATA) = struct
  type data = D.t
  type reg = A.reg

  type t =
    { r0 : D.t
    ; r1 : D.t
    ; r2 : D.t
    ; r3 : D.t
    ; r4 : D.t
    ; r5 : D.t
    ; r6 : D.t
    ; r7 : D.t
    }

  let empty =
    { r0 = D.undef
    ; r1 = D.undef
    ; r2 = D.undef
    ; r3 = D.undef
    ; r4 = D.undef
    ; r5 = D.undef
    ; r6 = D.undef
    ; r7 = D.undef
    }
  ;;

  let to_string r =
    Printf.sprintf
      "[%d %d %d %d %d %d %d %d]"
      (D.to_int r.r0)
      (D.to_int r.r1)
      (D.to_int r.r2)
      (D.to_int r.r3)
      (D.to_int r.r4)
      (D.to_int r.r5)
      (D.to_int r.r6)
      (D.to_int r.r7)
  ;;

  let read i r =
    match i with
    | Data.A.R0 -> r.r0
    | Data.A.R1 -> r.r1
    | Data.A.R2 -> r.r2
    | Data.A.R3 -> r.r3
    | Data.A.R4 -> r.r4
    | Data.A.R5 -> r.r5
    | Data.A.R6 -> r.r6
    | Data.A.R7 -> r.r7
  ;;

  let write i v r =
    match i with
    | Data.A.R0 -> { r with r0 = v }
    | Data.A.R1 -> { r with r1 = v }
    | Data.A.R2 -> { r with r2 = v }
    | Data.A.R3 -> { r with r3 = v }
    | Data.A.R4 -> { r with r4 = v }
    | Data.A.R5 -> { r with r5 = v }
    | Data.A.R6 -> { r with r6 = v }
    | Data.A.R7 -> { r with r7 = v }
  ;;
end
