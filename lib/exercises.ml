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

type 'a node = One of 'a | ManyL of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
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

let decode_rle lst =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many (n, x) :: xs -> aux (many acc n x) xs
  in
  aux [] (List.rev lst)

let direct_rle lst =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> rle count x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 (rle count a :: acc) t
  in
  List.rev (aux 0 [] lst)

let rec duplicate = function [] -> [] | x :: xs -> x :: x :: duplicate xs

let replicate lst n =
  let rec copy acc x n = if n = 0 then acc else copy (x :: acc) x (n - 1) in
  let rec aux acc = function [] -> acc | x :: xs -> aux (copy acc x n) xs in
  aux [] (List.rev lst)

let drop lst n =
  let rec aux i = function
    | [] -> []
    | x :: xs -> if i = n then aux 1 xs else x :: aux (i + 1) xs
  in
  aux 1 lst

let split lst n =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | x :: xs as lst ->
        if i = 0 then (List.rev acc, lst) else aux (i - 1) (x :: acc) xs
  in
  aux n [] lst

let slice lst i k =
  let rec drop n = function
    | [] -> []
    | _ :: xs as l -> if n = 0 then l else drop (n - 1) xs
  in
  let rec take n = function
    | [] -> []
    | x :: xs -> if n = 0 then [] else x :: take (n - 1) xs
  in
  drop i lst |> take (k - i + 1)

let rotate lst n =
  let rev_concat (a, b) = b @ a in
  rev_concat @@ split lst (n mod List.length lst)

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

let rand_select lst n =
  Random.init 0;
  let length = List.length lst in
  let rec extract acc i = function
    | [] -> raise Not_found
    | x :: xs -> if i = 0 then (x, acc @ xs) else extract (x :: acc) (i - 1) xs
  in
  let extract_rand lst len = extract [] (Random.int len) lst in
  let rec aux n acc lst len =
    if n = 0 then acc
    else
      let selected, rest = extract_rand lst len in
      aux (n - 1) (selected :: acc) rest (len - 1)
  in
  aux (min n length) [] lst length
