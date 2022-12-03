module type REG = sig
  type t
  type addr
  type data

  val write : addr -> data -> t -> t
  val read : addr -> t -> data
  val empty : t
end

module Make (A : Data.ADDR with type t = Data.r) (D : Data.DATA) :
  REG with type data = D.t with type addr = A.t = struct
  module MA : Map.OrderedType with type t = A.t = struct
    include A

    let compare l r = to_int l - to_int r
  end

  module M = Map.Make (MA)

  type data = D.t
  type addr = A.t
  type t = D.t M.t

  let empty = M.empty

  let read r reg =
    match M.find_opt r reg with
    | Some x -> x
    | None -> D.undef
  ;;

  let write = M.add
end
