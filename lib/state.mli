(** [M] is memory module of Synacor arch. *)
module M : Mem.MEM with type data = Data.D.t with type addr = Data.A.t

(** [R] is reg module of Synacor arch. *)
module R : Reg.REG with type data = Data.D.t with type addr = Data.R.t

(** [state] is type of VM state, and is passed as internal state of
    state monad [t]. *)
type state =
  { pc : Data.A.t
  ; mem : M.t
  ; stack : Data.D.t Stack.t
  ; reg : R.t
  }

(** [t] is a state monad that represents internal VM state transition - e.g.
    writing to memory, or incrementing program counter. *)
type 'a t = state -> 'a * state

(** [bind] is monadic bind. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [return x state] is monadic return. This is the same as
    [return x = fun state -> x, state] *)
val return : 'a -> state -> 'a * state

(** [product] is applicative product. *)
val product : 'a t -> 'b t -> ('a * 'b) t

(** [map] is applicative map. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [get] returns a state as result. *)
val get : unit -> state t

(** [put] replaces state. *)
val put : state -> unit t

(** [let*] is monadic let operator. *)
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(* 'a t -> f:('a -> 'b t) -> 'b t *)

(** [let+] is applicative let operator. *)
val ( let+ ) : ('a -> 'b) -> 'a t -> 'b t

(** [and+] is applicative and operator. *)
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

(** [of_bytes bs] decodes [bs] to VM memory state. *)
val of_bytes : bytes -> state

(** [of_ints is] decodes integers [is] to VM memory state. *)
val of_ints : int list -> state
