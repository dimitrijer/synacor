type 'a t = 'a list

let empty = []
let push v s = v :: s

exception Stack_Empty

let pop = function
  | [] -> raise Stack_Empty
  | hd :: tl -> hd, tl
;;

let is_empty = function
  | [] -> true
  | _ -> false
;;

let%test "test push pop" = empty |> push 0 |> push 10 |> pop = (10, [ 0 ])
