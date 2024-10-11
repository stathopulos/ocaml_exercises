let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec last_two = function
  | [ x; y ] -> Some (x, y)
  | _ :: x :: xs -> last_two (x :: xs)
  | _ -> None
