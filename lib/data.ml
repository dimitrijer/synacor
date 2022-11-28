module type DATA = sig
  type t

  val undef : t
  val max : t
  val min : t
  val of_int : int -> t
  val to_int : t -> int
end

module type ADDR = sig
  include Map.OrderedType

  type reg =
    | R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7

  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
  val low : t
  val high : t
  val incr : t -> t
  val add : int -> t -> t
  val to_reg : t -> reg
  val to_reg_opt : t -> reg option
  val of_reg : reg -> t
end

module Impl = struct
  type t = int

  type reg =
    | R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7

  let compare = ( - )
  let low = 0
  let high = 32775 (* to allow register addressing *)

  let max = high
  let min = low
  let undef = Int.shift_left 1 16 - 1 (* invalid value that is still in 16-bit range *)

  (* Highest mem addr, register addresses in [mem_high, high] range. *)
  let mem_high = 32767
  let to_int x = x

  let of_int x =
    if x >= low && x <= high then x else failwith @@ Printf.sprintf "illegal value: %d" x
  ;;

  let add c x = of_int (to_int x + c)
  let incr x = add x 1

  let to_reg_opt x =
    match to_int x with
    | 32768 -> Some R0
    | 32769 -> Some R1
    | 32770 -> Some R2
    | 32771 -> Some R3
    | 32772 -> Some R4
    | 32773 -> Some R5
    | 32774 -> Some R6
    | 32775 -> Some R7
    | _ -> None
  ;;

  let to_string x =
    match to_reg_opt x with
    | Some _ -> Printf.sprintf "REG%d" (x - mem_high - 1)
    | None -> Int.to_string x
  ;;

  let to_reg x =
    match to_reg_opt x with
    | Some x -> x
    | None -> failwith @@ Printf.sprintf "invalid register addr: %d" (to_int x)
  ;;

  let of_reg = function
    | R0 -> of_int 32768
    | R1 -> of_int 32769
    | R2 -> of_int 32770
    | R3 -> of_int 32771
    | R4 -> of_int 32772
    | R5 -> of_int 32773
    | R6 -> of_int 32774
    | R7 -> of_int 32775
  ;;
end

module D : DATA = Impl
module A : ADDR = Impl
