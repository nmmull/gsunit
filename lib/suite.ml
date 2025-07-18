open Utils

type group =
  {
    name: string;
    max_score: float;
    tests: Test.t list;
  }

type test =
  | Single of Test.t
  | Group of group

let test_to_ounit_test = function
  | Single t -> Test.to_ounit_test t
  | Group g ->
     g.tests
     |> List.map Test.to_ounit_test
     |> OUnit2.test_list
     |> OUnit2.(>:) g.name

type t =
  {
    score: float option;
    execution_time: int option;
    output: string option;
    output_format: output_string_format;
    test_output_format: output_string_format;
    test_name_format: output_string_format;
    visibility: visibility;
    stdout_visibility: visibility;
    tests: test list;
  }

let of_tests
      ?score
      ?execution_time
      ?output
      ?(output_format=Text)
      ?(test_output_format=Text)
      ?(test_name_format=Text)
      ?(visibility=Visible)
      ?(stdout_visibility=Visible)
      tests =
  {
    score;
    execution_time;
    output;
    output_format;
    test_output_format;
    test_name_format;
    visibility;
    stdout_visibility;
    tests
  }

let to_ounit_test suite =
  suite.tests
  |> List.map test_to_ounit_test
  |> OUnit2.test_list
