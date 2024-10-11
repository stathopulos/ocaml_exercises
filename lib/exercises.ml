let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let rec my_nth lst n =
  match lst with
  | [] -> None
  | x :: xs -> if n = 0 then Some x else my_nth xs (n - 1)
