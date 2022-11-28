(** [DATA] interface represents a value. *)
module type DATA = sig
  (** [t] is type of value. *)
  type t

  (** [undef] is undefined value. Undefined values appear in uninitialized
      memory locations or registers. *)
  val undef : t

  (** [max] is maximum value. *)
  val max : t

  (** [min] is minimum value. *)
  val min : t

  (** [of_int x] converts integer [x] to data value. Throws an exception if
      [x > max || x < min]. *)
  val of_int : int -> t

  (** [to_int d] converts data [d] to integer. *)
  val to_int : t -> int
end

(** [ADDR] interface represents addresses and operations on them. *)
module type ADDR = sig
  (* Necessary for use in Maps. *)
  include Map.OrderedType

  (* Register type represents specific register. *)
  type reg =
    | R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7

  (** [of_int x] converts integer [x] into address. Throws an exception if
      [x > high || h < low]. *)
  val of_int : int -> t

  (** [to_int x] converts address [a] to integer. *)
  val to_int : t -> int

  (** [to_string a] is string representation of [a]. *)
  val to_string : t -> string

  (** [low] represents lowest address. *)
  val low : t

  (** [high] represents highest address. *)
  val high : t

  (** [incr a] increments address by one. Throwns an exception if resulting
      address would be higher than [high]. *)
  val incr : t -> t

  (** [add c a] adds [c] to address [a]. Throws an exception if address would
      be higher than [high]. *)
  val add : int -> t -> t

  (** [to_reg a] converts address [a] to register enum. Throws an exception if
      [a] is not register address. *)
  val to_reg : t -> reg

  (** [to_reg a] converts address [a] to register, returning [None] if [a]
      is not register address. *)
  val to_reg_opt : t -> reg option

  (** [of_reg r] converts register [r] into its address. *)
  val of_reg : reg -> t
end

(** [D] is a [DATA] module implementation for Synacor arch. *)
module D : DATA

(** [A] is a [ADDR] module implementation for Synacor arch. *)
module A : ADDR
