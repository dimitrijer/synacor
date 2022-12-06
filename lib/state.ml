type ('s, 'a) t = 's -> 'a * 's

let bind ma f state =
  let a, s' = ma state in
  f a s'
;;

let return a state = a, state

let product ma mb state =
  let a, s' = ma state in
  let b, s'' = mb s' in
  (a, b), s''
;;

let map f ma state =
  let a, s' = ma state in
  f a, s'
;;

let ( let* ) = bind
let ( let+ ) = map
let ( and+ ) = product
let get () state = state, state
let put s _ = (), s
