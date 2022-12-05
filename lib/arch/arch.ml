module Data_Impl = struct
  type t = int

  (* Invalid value that is still in 16-bit range. *)
  let undef = Int.shift_left 1 16 - 1

  (* Includes register address range. *)
  let max = 32775
  let min = 0
  let of_int_opt x = if x >= min && x <= max then Some x else None

  let of_int x =
    match of_int_opt x with
    | Some a -> a
    | None -> failwith @@ Printf.sprintf "illegal data: %d" x
  ;;

  let to_int x = x
  let modulo = 0x8000 (* 32768 *)
  let modulo_mask = 0x7FFF
  let add a b = (a + b) mod modulo
  let mult a b = a * b mod modulo
  let logand a b = Int.logand a b
  let logor a b = Int.logor a b

  let lognot a =
    let na = Int.lognot a in
    Int.logand na modulo_mask
  ;;

  let modu a b = a mod b
  let eq = Int.equal
  let gt a b = Int.compare a b > 0
end

module D : Data.DATA = Data_Impl

module Addr_Impl = struct
  type t = int

  let low = 0
  let high = 32767
  let to_int x = x
  let of_int_opt x = if x >= low && x <= high then Some x else None

  let of_int x =
    match of_int_opt x with
    | Some a -> a
    | None -> failwith @@ Printf.sprintf "illegal address: %d" x
  ;;

  let incr x = of_int (to_int x + 1)
end

module A : Data.ADDR = Addr_Impl

module Reg_Impl = struct
  type t = Reg.r

  let low = Reg.R0
  let high = Reg.R7

  let to_int = function
    | Reg.R0 -> 32768
    | Reg.R1 -> 32769
    | Reg.R2 -> 32770
    | Reg.R3 -> 32771
    | Reg.R4 -> 32772
    | Reg.R5 -> 32773
    | Reg.R6 -> 32774
    | Reg.R7 -> 32775
  ;;

  let of_int_opt = function
    | 32768 -> Some Reg.R0
    | 32769 -> Some Reg.R1
    | 32770 -> Some Reg.R2
    | 32771 -> Some Reg.R3
    | 32772 -> Some Reg.R4
    | 32773 -> Some Reg.R5
    | 32774 -> Some Reg.R6
    | 32775 -> Some Reg.R7
    | _ -> None
  ;;

  let of_int x =
    match of_int_opt x with
    | Some r -> r
    | None -> failwith @@ Printf.sprintf "illegal reg: %d" x
  ;;

  let incr x = of_int (to_int x + 1)
end

module RI : Data.ADDR with type t = Reg.r = Reg_Impl
module M = Mem.Make (A) (D)
module R = Reg.Make (RI) (D)
module S = Stack

type t =
  { pc : A.t
  ; mem : M.t
  ; stack : D.t S.t
  ; reg : R.t
  }
