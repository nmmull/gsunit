include Utils

module SubTest = SubTest
module Test = Test
module Group = Group
module Suite = Suite
module Gradescope = Gradescope

let subtest = SubTest.of_test_fun
let test = Test.of_case
let group = Group.of_tests

let run
      ?group_name_formatter
      ?output_formatter
      ?status_formatter
      ?(ounit_test_runner=default_ounit_test_runner)
      ?output
      ?visibility
      ?stdout_visibility
      ?extra_data
      suite =
  let suite =
    Suite.mk
      ?output
      ?visibility
      ?stdout_visibility
      ?extra_data
      suite
  in
  if Array.exists ((=) "-ounit") Sys.argv
  then
    ignore
      (default_ounit_test_runner ~debug:true ()
         (Suite.to_ounit_test suite))
  else
    let ounit_results =
      suite
      |> Suite.to_ounit_test
      |> ounit_test_runner ()
      |> reformat_ounit_results
    in
    let gradescope_results =
      suite
      |> Suite.test_to_result ounit_results
      |> Suite.to_gradescope
           ?group_name_formatter
           ?output_formatter
           ?status_formatter
      |> Gradescope.Suite.to_json
    in
    Yojson.Basic.to_file
      "ounit_gradescope_output.json"
      gradescope_results

let check
      ?test_name
      ?hint
      ?hidden
      ~printer
      fn
      fn_name
      (i, e) =
  let name =
    match test_name with
    | Some name -> name
    | None -> Printf.sprintf "%s %s = %s" fn_name (printer i) (printer e)
  in
  let test_fun _ =
    let a = fn i in
    let msg =
      Printf.sprintf
        "test name: %s\nfunction: %s\ninput: %s\nexpected: %s\nactual: %s"
        name
        fn_name
        i
        e
        a
    in OUnit2.assert_equal ~msg e a
  in
  test
    ?hidden
    ?hint
    ~name
    (`Single test_fun)
