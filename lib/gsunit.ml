include Utils

module SubTest = SubTest
module Test = Test
module Group = Group
module Suite = Suite
module Gradescope = Gradescope

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

let test_fun
    ?timeout
    ?cmp
    ~pp_in
    ~pp_out
    fn
    fn_name
    input
    expected =
  match timeout with
  | None -> fun _ ->
    let actual = fn input in
    let msg =
      Format.asprintf
        "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
        fn_name
        pp_in input
        pp_out expected
        pp_out actual
    in OUnit2.assert_equal ?cmp ~msg expected actual
  | Some timeout -> fun _ -> (* silly, but works for now I think? *)
    match Unix.fork () with
    | 0 ->
      let _actual = fn input in
      Unix._exit 0
    | child_pid ->
      let deadline = Unix.gettimeofday () +. timeout in
      let rec loop () =
        match Unix.waitpid [Unix.WNOHANG] child_pid with
        | 0, _ ->
          if Unix.gettimeofday () > deadline
          then
            let _kill = try Unix.kill child_pid Sys.sigkill with _ -> () in
            let _reap = Unix.waitpid [] child_pid in
            OUnit2.assert_failure "Timed out"
          else
            let _sleep = Unix.sleepf 0.05 in
            loop ()
        | _, Unix.WEXITED 0 ->
          let actual = fn input in
          let msg =
            Format.asprintf
              "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
              fn_name
              pp_in input
              pp_out expected
              pp_out actual
          in OUnit2.assert_equal ?cmp ~msg expected actual
        | _ -> OUnit2.assert_failure "Something went wrong"
      in loop ()

let check
    ?name
    ?timeout
    ?cmp
    ~pp_in
    ~pp_out
    fn
    fn_name
    input
    expected =
  let test_fun =
    test_fun
      ?timeout
      ?cmp
      ~pp_in
      ~pp_out
      fn
      fn_name
      input
      expected
  in test ?name (`Single test_fun)

let check_ref
    ?name
    ?timeout
    ?cmp
    ~pp_in
    ~pp_out
    fn
    fn_name
    fn_ref
    input =
  let test_fun =
    let expected = fn_ref input in
    test_fun
      ?timeout
      ?cmp
      ~pp_in
      ~pp_out
      fn
      fn_name
      input
      expected
  in test ?name (`Single test_fun)

let check_sub
    ?name
    ?timeout
    ?cmp
    ~pp_in
    ~pp_out
    fn
    fn_name
    input
    expected =
  let test_fun =
    test_fun
      ?timeout
      ?cmp
      ~pp_in
      ~pp_out
      fn
      fn_name
      input
      expected
  in subtest ?name test_fun

let check_sub_ref
    ?name
    ?timeout
    ?cmp
    ~pp_in
    ~pp_out
    fn
    fn_name
    fn_ref
    input =
  let test_fun =
    let expected = fn_ref input in
    test_fun
      ?timeout
      ?cmp
      ~pp_in
      ~pp_out
      fn
      fn_name
      input
      expected
  in subtest ?name test_fun
