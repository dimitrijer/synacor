(** [MEM] is interface representing memory. *)
module type MEM = sig
  (** [t] is memory type. *)
  type t

  (** [addr] is address type, used for addressing words stored in memory. *)
  type addr

  (** [data] is data type that represents words stored in memory. *)
  type data

  (** [empty] returns an empty memory. *)
  val empty : t

  (** [read a m] reads word at address [a] from [m]. *)
  val read : addr -> t -> data

  (** [write a w m] writes word [w] at address [a] to [m]. *)
  val write : addr -> data -> t -> t
end

(** [Make] functor creates a concrete memory module for given address [A] and
    data [D] module implementations. *)
module Make (A : Data.ADDR) (D : Data.DATA) :
  MEM with type addr = A.t with type data = D.t
