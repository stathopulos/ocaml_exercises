open OUnit2
open Ocaml_exercises.Exercises

let lst = [ "a"; "b"; "c"; "d" ]
let tail_of_list _ = assert_equal (Some "d") (last lst)
let last_two_of_list _ = assert_equal (Some ("c", "d")) (last_two lst)
let nth_element _ = assert_equal (Some "c") (my_nth lst 2)
let list_length _ = assert_equal 4 (length lst)
let rev_list _ = assert_equal [ "d"; "c"; "b"; "a" ] (rev lst)
let list_palindrome _ = assert_equal true (is_palindrome [ "d"; "a"; "d" ])

let rle_list _ =
  assert_equal
    [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]
    (rle
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])

let modified_rle _ =
  assert_equal
    [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]
    (mod_rle
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])

let duplicate_list _ =
  assert_equal
    [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
    (duplicate [ "a"; "b"; "c"; "c"; "d" ])

let split_prefix _ =
  assert_equal
    ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
    (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3)

let remove_element _ =
  assert_equal [ "a"; "c"; "d" ] (remove_at 1 [ "a"; "b"; "c"; "d" ])

let insert_element _ =
  assert_equal
    [ "a"; "alfa"; "b"; "c"; "d" ]
    (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ])

let range_of_ints _ = assert_equal [ 4; 5; 6; 7; 8; 9 ] (range 4 9)

let flatten_list _ =
  assert_equal
    [ "a"; "b"; "c"; "d"; "e" ]
    (flatten
       [ OneL "a"; ManyL [ OneL "b"; ManyL [ OneL "c"; OneL "d" ]; OneL "e" ] ])

let tests =
  "Exercise Tests"
  >::: [
         "tail_of_list" >:: tail_of_list;
         "last_two_of_list" >:: last_two_of_list;
         "nth_element" >:: nth_element;
         "list_length" >:: list_length;
         "rev_list" >:: rev_list;
         "list_palindrome" >:: list_palindrome;
         "rle_list" >:: rle_list;
         "modified_rle" >:: modified_rle;
         "duplicate_list" >:: duplicate_list;
         "split_prefix" >:: split_prefix;
         "remove_element" >:: remove_element;
         "insert_element" >:: insert_element;
         "range_of_ints" >:: range_of_ints;
         "flatten_list" >:: flatten_list;
       ]

let () = run_test_tt_main tests
