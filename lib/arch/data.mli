(** [DATA] interface represents a value that is stored or read in memory,
    registers and stack. *)
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

  (** [of_int_opt x] converts integer [x] to data value. Returns None if
      [x > (to_int high) || x < (to_int low)]. *)
  val of_int_opt : int -> t option

  (** [to_int d] converts data [d] to integer. *)
  val to_int : t -> int

  (** [modulo] is the value that arithmetic operations on data wrap around. *)
  val modulo : t

  (** [add a b] adds [a] to [b] modulo [modulo_max]. *)
  val add : t -> t -> t

  (** [equal a b] returs true if [a == b]. *)
  val equal : t -> t -> bool
end

(** [ADDR] interface represents memory addresses and operations on them. *)
module type ADDR = sig
  (* include Base.Comparable.S with type t := int *)

  (** [t] is type of address. *)
  type t

  (** [of_int x] converts integer [x] into address. Throws an exception if
      [x > (to_int high) || x < (to_int low)]. *)
  val of_int : int -> t

  (** [of_int_opt x] converts integer [x] into address. Returns None if
      [x > (to_int high) || x < (to_int low)]. *)
  val of_int_opt : int -> t option

  (** [to_int x] converts address [a] to integer. *)
  val to_int : t -> int

  (** [low] represents lowest address. *)
  val low : t

  (** [high] represents highest address. *)
  val high : t

  (** [incr a] increments address by one. Throws an exception if resulting
      address would be higher than [high]. *)
  val incr : t -> t
end
