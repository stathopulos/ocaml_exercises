open OUnit2
open Ocaml_exercises.Exercises

let tail_of_list _ = assert_equal (Some "d") (last [ "a"; "b"; "c"; "d" ])
let tests = "Exercise Tests" >::: [ "tail_of_list" >:: tail_of_list ]
let () = run_test_tt_main tests
