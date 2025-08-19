include Utils

module SubTest = SubTest
module Test = Test
module Group = Group
module Suite = Suite
module Gradescope = Gradescope

type result_formatter = SubTest.result list -> formatted_string option

type group_name_formatter = string option -> formatted_string -> formatted_string

let subtest = SubTest.of_test_fun
let test = Test.of_case
let group = Group.of_tests
let suite = Suite.mk

let run
    ?group_name_formatter
    ?output_formatter
    ?status_formatter
    ?(ounit_test_runner=default_ounit_test_runner)
    suite =
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
      ?name
      ?hint
      ?hidden
      ~pp_in
      ~pp_out
      fn
      fn_name
      input
      expected =
  let test_fun _ =
    let actual = fn input in
    let msg =
      Format.asprintf
        "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
        fn_name
        pp_in input
        pp_out expected
        pp_out actual
    in OUnit2.assert_equal ~msg expected actual
  in
  test
    ?hidden
    ?hint
    ?name
    (`Single test_fun)

let check_ref
    ?name
    ?hint
    ?hidden
    ~pp_in
    ~pp_out
    fn
    fn_name
    fn_ref
    input =
  let test_fun _ =
    let expected = fn_ref input in
    let actual = fn input in
    let msg =
      Format.asprintf
        "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
        fn_name
        pp_in input
        pp_out expected
        pp_out actual
    in OUnit2.assert_equal ~msg expected actual
  in
  test
    ?hidden
    ?hint
    ?name
    (`Single test_fun)

let check_sub
    ?name
    ?hint
    ?hidden
    ~pp_in
    ~pp_out
    fn
    fn_name
    input
    expected =
  let test_fun _ =
      let actual = fn input in
      let msg =
        Format.asprintf
          "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
          fn_name
          pp_in input
          pp_out expected
          pp_out actual
      in OUnit2.assert_equal ~msg expected actual
  in
  subtest
    ?name
    ?hint
    ?hidden
    test_fun

let check_sub_ref
    ?name
    ?hint
    ?hidden
    ~pp_in
    ~pp_out
    fn
    fn_name
    fn_ref
    input =
  let test_fun _ =
    let expected = fn_ref input in
    let actual = fn input in
    let msg =
      Format.asprintf
        "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
        fn_name
        pp_in input
        pp_out expected
        pp_out actual
    in OUnit2.assert_equal ~msg expected actual
  in
  subtest
    ?hidden
    ?hint
    ?name
    test_fun
