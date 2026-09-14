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

let check
    ?name
    ?cmp
    ~pp_in
    ~pp_out
    fn
    fn_name
    input
    expected =
  let test_fun _ =
    let read_fd, write_fd = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
      let _close_read = Unix.close read_fd in
      let out_chan = Unix.out_channel_of_descr write_fd in
      let _calculate_actual =
        match Marshal.to_channel out_chan (fn input) [] with
        | _ -> flush out_chan
        | exception _ -> ()
      in
      Unix._exit 0
    | child_pid ->
      let _close_write = Unix.close write_fd in
      let in_chan = Unix.in_channel_of_descr read_fd in
      let deadline = Unix.gettimeofday () +. 2.0 in
      let rec loop () =
        match Unix.waitpid [Unix.WNOHANG] child_pid with
        | 0, _ ->
          if Unix.gettimeofday () > deadline
          then
            let _kill = try Unix.kill child_pid Sys.sigkill with _ -> () in
            let _dunno = Unix.waitpid [] child_pid in
            OUnit2.assert_failure "Timed out"
          else
            let _sleep = Unix.sleepf 0.05 in
            loop ()
        | _, Unix.WEXITED 0 ->
          let actual = Marshal.from_channel in_chan in
          let msg =
            Format.asprintf
              "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
              fn_name
              pp_in input
              pp_out expected
              pp_out actual
          in OUnit2.assert_equal ~msg ?cmp expected actual
        | _ -> OUnit2.assert_failure "Something went wrong"
      in loop ()
  in
  test
    ?name
    (`Single test_fun)

let check_ref
    ?name
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
    ?name
    (`Single test_fun)

let check_sub
    ?name
    ~pp_in
    ~pp_out
    fn
    fn_name
    input
    expected =
  let test_fun _ =
    let read_fd, write_fd = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
      let _close_read = Unix.close read_fd in
      let out_chan = Unix.out_channel_of_descr write_fd in
      let _calculate_actual =
        match Marshal.to_channel out_chan (fn input) [] with
        | _ -> flush out_chan
        | exception _ -> ()
      in
      Unix._exit 0
    | child_pid ->
      let _close_write = Unix.close write_fd in
      let in_chan = Unix.in_channel_of_descr read_fd in
      let deadline = Unix.gettimeofday () +. 2.0 in
      let rec loop () =
        match Unix.waitpid [Unix.WNOHANG] child_pid with
        | 0, _ ->
          if Unix.gettimeofday () > deadline
          then
            let _kill = try Unix.kill child_pid Sys.sigkill with _ -> () in
            let _dunno = Unix.waitpid [] child_pid in
            OUnit2.assert_failure "Timed out"
          else
            let _sleep = Unix.sleepf 0.05 in
            loop ()
        | _, Unix.WEXITED 0 ->
          let actual = Marshal.from_channel in_chan in
          let msg =
            Format.asprintf
              "function: %s@.input:@[<hv>@;<1 2>%a@]@.expected:@[<hv>@;<1 2>%a@]@.actual:@[<hv>@;<1 2>%a@]@."
              fn_name
              pp_in input
              pp_out expected
              pp_out actual
          in OUnit2.assert_equal ~msg expected actual
        | _ -> OUnit2.assert_failure "Something went wrong"
      in loop ()
  in
  subtest
    ?name
    test_fun

let check_sub_ref
    ?name
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
    ?name
    test_fun
