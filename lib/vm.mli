(** [of_bytes bs] decodes [bs] to VM memory state. *)
val of_bytes : ?inbuf:string -> bytes -> Arch.t

(** [of_ints is] decodes integers [is] to VM memory state. *)
val of_ints : int list -> Arch.t

(** [run s] runs VM with initial state [s], and returns final state. *)
val run : Arch.t -> Arch.t
