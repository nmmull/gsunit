include Utils

module SubTest = SubTest
module Test = Test
module Group = Group
module Suite = Suite
module Gradescope = Gradescope

let run
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
  then OUnit2.run_test_tt_main (Suite.to_ounit_test suite)
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
      (* |> reformat_output *)
      |> Suite.to_gradescope
      |> Gradescope.Suite.to_json
    in
    Yojson.Basic.to_file
      "ounit_gradescope_output.json"
      gradescope_results
