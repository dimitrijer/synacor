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
  type r = A.reg

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

  let to_string reg =
    Printf.sprintf
      "[%d %d %d %d %d %d %d %d]"
      (D.to_int reg.r0)
      (D.to_int reg.r1)
      (D.to_int reg.r2)
      (D.to_int reg.r3)
      (D.to_int reg.r4)
      (D.to_int reg.r5)
      (D.to_int reg.r6)
      (D.to_int reg.r7)
  ;;

  let read r reg =
    match r with
    | A.R0 -> reg.r0
    | A.R1 -> reg.r1
    | A.R2 -> reg.r2
    | A.R3 -> reg.r3
    | A.R4 -> reg.r4
    | A.R5 -> reg.r5
    | A.R6 -> reg.r6
    | A.R7 -> reg.r7
  ;;

  let write r v reg =
    match r with
    | A.R0 -> { reg with r0 = v }
    | A.R1 -> { reg with r1 = v }
    | A.R2 -> { reg with r2 = v }
    | A.R3 -> { reg with r3 = v }
    | A.R4 -> { reg with r4 = v }
    | A.R5 -> { reg with r5 = v }
    | A.R6 -> { reg with r6 = v }
    | A.R7 -> { reg with r7 = v }
  ;;
end
