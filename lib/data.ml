module type DATA = sig
  type t

  val undef : t
  val max : t
  val min : t
  val of_int : int -> t
  val of_int_opt : int -> t option
  val to_int : t -> int
  val modulo : t
  val add : t -> t -> t
end

module type ADDR = sig
  type t

  val of_int : int -> t
  val of_int_opt : int -> t option
  val to_int : t -> int
  val low : t
  val high : t
  val incr : t -> t
end

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
  let modulo = 32768
  let add a b = (a + b) mod modulo
end

module D : DATA = Data_Impl

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

module A : ADDR = Addr_Impl

type r =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7

module Reg_Impl = struct
  type t = r

  let low = R0
  let high = R7

  let to_int = function
    | R0 -> 32768
    | R1 -> 32769
    | R2 -> 32770
    | R3 -> 32771
    | R4 -> 32772
    | R5 -> 32773
    | R6 -> 32774
    | R7 -> 32775
  ;;

  let of_int_opt = function
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

  let of_int x =
    match of_int_opt x with
    | Some r -> r
    | None -> failwith @@ Printf.sprintf "illegal reg: %d" x
  ;;

  let incr x = of_int (to_int x + 1)
end

module R : ADDR with type t = r = Reg_Impl
