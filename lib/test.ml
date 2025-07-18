open Utils

type sub_test =
  | Single of OUnitTest.test_fun
  | Multi of Sub_test.t list

type output_formatter = (int * OUnitTest.result) list -> string option * output_string_format

let default_output_formatter _ = (None, Text)

type t =
  {
    max_score: float;
    name: string option;
    hint: string option;
    hidden: bool;
    name_format: output_string_format;
    number: string option;
    output_formatter : output_formatter;
    tags: string list;
    visibility: visibility;
    sub_test: sub_test;
  }

let of_sub_tests
      ?max_score
      ?name
      ?hint
      ?(hidden=false)
      ?(name_format=Text)
      ?number
      ?(output_formatter=default_output_formatter)
      ?(tags=[])
      ?(visibility=Visible)
      sub_test =
  let max_score =
    match max_score with
    | None -> (
      match sub_test with
      | Single _ -> 1.0
      | Multi sub_tests -> float_of_int (List.length sub_tests)
    )
    | Some max_score -> max_score
  in
  {
    max_score;
    name;
    hint;
    hidden;
    name_format;
    number;
    output_formatter;
    tags;
    visibility;
    sub_test;
  }

let to_ounit_test test =
  let open OUnit2 in
  let ounit_test =
    match test.sub_test with
    | Single test_fun -> test_case test_fun
    | Multi sub_tests ->
       sub_tests
       |> List.map Sub_test.to_ounit_test_case
       |> test_list
  in
  match test.name with
  | None -> ounit_test
  | Some name -> name >: ounit_test
