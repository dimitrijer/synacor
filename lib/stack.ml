type 'a t = 'a list

let empty = []
let push v s = v :: s

let pop = function
  | [] -> failwith "stack empty"
  | hd :: tl -> hd, tl
;;

let%test "test push pop" = empty |> push 0 |> push 10 |> pop = (10, [ 0 ])
