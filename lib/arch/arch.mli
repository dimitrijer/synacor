(** [D] is [DATA] module implementation for Synacor arch data. *)
module D : Data.DATA

(** [A] is [ADDR] module implementation for Synacor arch memory addresses. *)
module A : Data.ADDR

(** [RI] is [ADDR] module implementation for Synacor arch register identifiers. *)
module RI : Data.ADDR with type t = Reg.r

(** [M] is the memory module of Synacor arch. *)
module M : Mem.MEM with type data = D.t with type addr = A.t

(** [R] is the register module of Synacor arch. *)
module R : Reg.REG with type data = D.t with type addr = RI.t

(** [S] is the stack module of Synacor arch. *)
module S : module type of Stack

(** [t] is type of VM state, and should be passed as internal state of
    state monad [State.t]. *)
type t =
  { pc : A.t (* program counter *)
  ; mem : M.t
  ; stack : D.t S.t
  ; reg : R.t
  }
