module type MEM = sig
  type t
  type addr
  type data

  val empty : t
  val read : addr -> t -> data
  val write : addr -> data -> t -> t
end

module Make (A : Data.ADDR) (D : Data.DATA) :
  MEM with type addr = A.t with type data = D.t = struct
  (* Address module that can be used as map key. *)
  module M_A : Map.OrderedType with type t = A.t = struct
    include A

    let compare l r = to_int l - to_int r
  end

  module M = Map.Make (M_A)

  type t = D.t M.t
  type data = D.t
  type addr = A.t

  let empty = M.empty

  let read a mem =
    match M.find_opt a mem with
    | Some x -> x
    | None -> D.undef
  ;;

  let write a v t = M.add a v t
end
