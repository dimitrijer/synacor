(** [t] is type of VM. VM is a State monad, where type represents a state
    transition, e.g fetching or executing an instruction. *)
type 'a t

(** [state] is state type of VM state monad. *)
type state

(** [run s] runs VM with initial state [s], and returns final state. *)
val run : state -> unit * state

(** [of_bytes bs] constructs a state from provided [bs]. *)
val of_bytes : bytes -> state
