let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t

let rec my_nth lst n =
  match lst with
  | [] -> None
  | x :: xs -> if n = 0 then Some x else my_nth xs (n - 1)

let length lst =
  let rec aux n = function [] -> n | _ :: xs -> aux (n + 1) xs in
  aux 0 lst
(* let length lst = List.fold_left (fun acc _ -> acc + 1) 0 lst *)

let rev lst =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: acc) xs in
  aux [] lst
(* let rev lst = List.fold_left (fun acc a -> a :: acc) [] lst *)
