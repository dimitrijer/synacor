(** Stack type. *)
type 'a t

exception Stack_Empty

(** [empty] returns an empty stack. *)
val empty : 'a t

(** [push x s] pushes [x] on top of stack [s]. *)
val push : 'a -> 'a t -> 'a t

(** [pop s] pops value from the top of [s], and returns that value and popped
    stack. It throws an exception if [s] is empty. *)
val pop : 'a t -> 'a * 'a t
