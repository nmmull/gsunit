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
      (default_ounit_test_runner
         ~debug:true
         ()
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
    Out_channel.with_open_text
      "ounit_gradescope_output.json"
      (fun out_channel ->
         Yojson.Basic.pretty_to_channel
           out_channel
           gradescope_results)

let check
      ?test_name
      ?hint
      ?hidden
      ~in_printer
      ~out_printer
      fn
      fn_name
      input
      expected =
  let name =
    match test_name with
    | Some name -> name
    | None ->
      (* TODO: FORMAT *)
      Printf.sprintf
        "%s %s = %s"
        fn_name
        (in_printer input)
        (out_printer expected)
  in
  let test_fun _ =
    let actual = fn input in
    let msg =
      (* TODO: FORMAT *)
      Printf.sprintf
        "function: %s\ninput: %s\nexpected: %s\nactual: %s"
        fn_name
        (in_printer input)
        (out_printer expected)
        (out_printer actual)
    in OUnit2.assert_equal ~msg expected actual
  in
  test
    ?hidden
    ?hint
    ~name
    (`Single test_fun)

let check_ref = assert false

let check_list = assert false

let check_list_ref = assert false
