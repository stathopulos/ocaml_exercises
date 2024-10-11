open OUnit2
open Ocaml_exercises.Exercises

let lst = [ "a"; "b"; "c"; "d" ]
let tail_of_list _ = assert_equal (Some "d") (last lst)
let last_two_of_list _ = assert_equal (Some ("c", "d")) (last_two lst)

let tests =
  "Exercise Tests"
  >::: [
         "tail_of_list" >:: tail_of_list;
         "last_two_of_list" >:: last_two_of_list;
       ]

let () = run_test_tt_main tests
