open OUnit2
open Ocaml_exercises.Exercises

let lst = [ "a"; "b"; "c"; "d" ]
let tail_of_list _ = assert_equal (Some "d") (last lst)
let last_two_of_list _ = assert_equal (Some ("c", "d")) (last_two lst)
let nth_element _ = assert_equal (Some "c") (my_nth lst 2)
let list_length _ = assert_equal 4 (length lst)
let rev_list _ = assert_equal [ "d"; "c"; "b"; "a" ] (rev lst)
let list_palindrome _ = assert_equal true (is_palindrome [ "d"; "a"; "d" ])

let tests =
  "Exercise Tests"
  >::: [
         "tail_of_list" >:: tail_of_list;
         "last_two_of_list" >:: last_two_of_list;
         "nth_element" >:: nth_element;
         "list_length" >:: list_length;
         "rev_list" >:: rev_list;
         "list_palindrome" >:: list_palindrome;
       ]

let () = run_test_tt_main tests
