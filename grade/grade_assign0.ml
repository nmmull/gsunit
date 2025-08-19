
module Sqrt = struct
  let name = "sqrt"
  let max_score = 5.0
  let sqrt = Assign0.sqrt

  let tests =
    let t =
      Gsunit.check
        ~pp_in:Fmt.int
        ~pp_out:Fmt.int
        sqrt "sqrt"
    in
    Gsunit.group
      ~name
      ~max_score
      [
        t 4 1;
        t 9 3;
        t 100 10;
        t 2 2;
        t 10 4;
        t 99 10;
      ]
end

module Is_prime = struct
  let name = "is_prime"
  let max_score = 5.
  let is_prime = Assign0.is_prime

  let test =
    let t =
      Gsunit.check_sub
           ~pp_in:Fmt.int
           ~pp_out:Fmt.bool
           is_prime
           "is_prime"
    in
    Gsunit.test
      ~name
      ~max_score
      (`Multi
         [
           t 0 false;
           t 1 false;
           t 2 true;
           t 37 true;
           t 57 true;
           t 97 true;
         ])

  let tests =
    Gsunit.group [ test ]
end

let () =
  [
    Sqrt.tests;
    Is_prime.tests;
  ]
  |> Gsunit.suite
  |> Gsunit.run

(* let grade_sqrt = *)
(*   let test i = *)
(*     let description = "(HIDDEN) testing sqrt " ^ string_of_int i in *)
(*     let case _ = *)
(*       let expected = Ref.sqrt i in *)
(*       let actual = Assign0.sqrt i in *)
(*       assert_equal expected actual *)
(*     in *)
(*     description >: gs_test_case case *)
(*   in *)
(*   "grading sqrt" >::: List.init 100 test *)

(* let grade_is_prime = *)
(*   let test i = *)
(*     let description = "(HIDDEN) testing prime " ^ string_of_int i in *)
(*     let expected = Ref.is_prime i in *)
(*     let actual = Assign0.is_prime i in *)
(*     let case _ = assert_equal expected actual in *)
(*     description >: gs_test_case case *)
(*   in *)
(*   "grading is_prime" >::: List.init 100 test *)

(* let tests = *)
(*   [ *)
(*     test_sqrt, 3; *)
(*     test_is_prime, 3; *)
(*     grade_sqrt, 7; *)
(*     grade_is_prime, 7; *)
(*   ] *)

(* let _ = run_tests_gradescope tests *)
