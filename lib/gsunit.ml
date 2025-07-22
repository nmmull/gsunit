open Utils

let run
      ?(debug=false)
      ?(ounit_test_runner=default_ounit_test_runner)
      suite =
  let open Suite in
  if debug
  then OUnit2.run_test_tt_main (to_ounit_test suite)
  else
    let ounit_results =
      suite
      |> to_ounit_test
      |> ounit_test_runner
      |> reformat_ounit_results
    in
    let gradescope_results =
      suite
      |> test_to_result ounit_results
      |> to_gradescope
      |> Gradescope.Suite.to_json
    in
    Yojson.Basic.to_file
      "ounit_gradescope_output.json"
      gradescope_results
