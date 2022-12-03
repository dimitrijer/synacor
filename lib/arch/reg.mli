(* [r] is register identifier type for Synacor arch. *)
type r =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7

(** [REG] is interface that represents registers. *)
module type REG = sig
  (** [t] is type of registers. *)
  type t

  (** [addr] is type of register identifier. *)
  type addr

  (** [data] is type of data values written and read from registers. *)
  type data

  (** [write r w reg] writes word [w] to register [r] of registers [reg]. *)
  val write : addr -> data -> t -> t

  (** [read r reg] reads register [r] of registers [reg]. *)
  val read : addr -> t -> data

  (** [empty] returns empty registers with uninitialized data within. *)
  val empty : t
end

(** [Make] functor creates a concrete registers module for given address [A]
    and data [D] module implementations. *)
module Make (A : Data.ADDR with type t = r) (D : Data.DATA) :
  REG with type data = D.t with type addr = A.t
