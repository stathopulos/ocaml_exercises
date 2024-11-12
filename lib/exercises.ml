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

let is_palindrome lst = rev lst = lst
(* let is_palindrome lst = List.rev lst = lst *)

let rle lst =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] lst)

type 'a rle = One of 'a | Many of int * 'a
let mod_rle lst =
  let make_tuple count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> make_tuple (count + 1) x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (make_tuple (count + 1) a :: acc) t
  in
  List.rev (aux 0 [] lst)

let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (x::x::acc) xs
in
List.rev (aux [] lst)