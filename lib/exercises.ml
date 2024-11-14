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

let rec duplicate = function [] -> [] | x :: xs -> x :: x :: duplicate xs

let split lst n =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | x :: xs as lst ->
        if i = 0 then (List.rev acc, lst) else aux (i - 1) (x :: acc) xs
  in
  aux n [] lst

let rec remove_at i = function
  | [] -> []
  | x :: xs -> if i = 0 then xs else x :: remove_at (i - 1) xs

let rec insert_at e i = function
  | [] -> []
  | x :: xs as lst -> if i = 0 then e :: lst else x :: insert_at e (i - 1) xs

let rec range b t =
  if b = t then [ b ]
  else if b > t then b :: range (b - 1) t
  else b :: range (b + 1) t

type 'a node = OneL of 'a | ManyL of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | OneL x :: t -> aux (x :: acc) t
    | ManyL l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] lst)

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] lst)
