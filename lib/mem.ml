module type MEM = sig
  type t
  type addr
  type data

  val empty : t
  val read : addr -> t -> data
  val write : addr -> data -> t -> t
end

module Make (A : Data.ADDR) (D : Data.DATA) = struct
  module M = Map.Make (A)

  type t = D.t M.t
  type data = D.t
  type addr = A.t

  let empty = M.empty

  let read a mem =
    match M.find_opt a mem with
    | None -> D.undef
    | Some x -> x
  ;;

  let write a v t = M.add a v t
end
