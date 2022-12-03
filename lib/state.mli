(** [t] is a state monad that represents internal VM state transition - e.g.
    writing to memory, or incrementing program counter. ['s] is type of state
    that is threaded through state transitions, and ['a] is type of the result
    of state transition. *)
type ('s, 'a) t = 's -> 'a * 's

(** [bind] is monadic bind. *)
val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t

(** [return x state] is monadic return. This is the same as
    [return x = fun state -> x, state] *)
val return : 'a -> 's -> 'a * 's

(** [product] is applicative product. *)
val product : ('s, 'a) t -> ('s, 'b) t -> ('s, 'a * 'b) t

(** [map] is applicative map. *)
val map : ('a -> 'b) -> ('s, 'a) t -> ('s, 'b) t

(** [get] returns a state as result. *)
val get : unit -> ('s, 's) t

(** [put] replaces state. *)
val put : 's -> ('s, unit) t

(** [let*] is monadic let operator. *)
val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t

(** [let+] is applicative let operator. *)
val ( let+ ) : ('a -> 'b) -> ('s, 'a) t -> ('s, 'b) t

(** [and+] is applicative and operator. *)
val ( and+ ) : ('s, 'a) t -> ('s, 'b) t -> ('s, 'a * 'b) t
