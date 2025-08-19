open OUnit2

let rec num_cases otest =
  let open OUnitTest in
  match otest with
  | TestCase _ -> 1
  | TestList tests ->
     List.fold_left (+) 0
       (List.map num_cases tests)
  | TestLabel (_, test) -> num_cases test

module Test_info = struct
  type t =
    { label : string
    ; max_score : int
    ; num_cases : int
    }

  let of_test otest max_score =
    let open OUnitTest in
    let label =
      match otest with
      | TestLabel (label, _) -> label
      | _ -> failwith "Top-level test suit must have label"
    in
    { label
    ; max_score
    ; num_cases = num_cases otest
    }

  let test_infos = List.rev_map (fun (x, y) -> of_test x y)
end

module Test_result = struct
  type t =
    { info : Test_info.t
    ; tests : (string * bool) list
    }

  let num_passed res =
    List.fold_left
      (fun acc (_, b) -> acc + if b then 1 else 0)
      0
      res.tests

  let score r =
    let max_score = float_of_int r.info.max_score in
    let num_passed = float_of_int (num_passed r) in
    let num_cases = float_of_int r.info.num_cases in
    if r.info.num_cases = 0
    then 0.
    else max_score *. num_passed /. num_cases

  let string_of_tests tests =
    let string_of_test (label, passed) =
      let passed =
        if passed
        then "(PASSED) "
        else "(FAILED) "
      in passed ^ label
    in
    let is_visible (label, _) =
      not (String.starts_with ~prefix:"(HIDDEN)" label)
    in
    tests
    |> List.filter is_visible
    |> List.map string_of_test
    |> String.concat "\\n"

  let to_json r =
    let body =
      String.concat ",\n"
        [
          "\"name\": " ^ "\"" ^ r.info.label ^ "\"";
          "\"max_score\": " ^ string_of_int r.info.max_score;
          "\"score\": " ^ Printf.sprintf "%.2f" (score r);
          "\"output\": " ^ "\"" ^ string_of_tests r.tests ^ "\"";
        ]
    in String.concat "\n" ["{";body;"}"]

  let incr (label, status) res =
    {res with tests = (label, status = OUnitTest.RSuccess) :: res.tests }
end

module Test_results = struct
  open Test_result

  type t = Test_result.t list

  let init_results =
    List.map (fun t_info -> {info = t_info; tests = []})

  let incr (label, sublabel, status) =
   let rec go = function
      | [] -> failwith ("Unknown test (should be impossible): " ^ label)
      | res :: l ->
        if res.info.label = label
        then Test_result.incr (sublabel, status) res :: l
        else res :: go l
    in go

  let of_statuses test_infos =
    List.fold_left
      (fun results status -> incr status results)
      (init_results test_infos)

  let to_json results =
    String.concat "\n"
      [ "{"
      ;  "\"output\": \"Submission accepted.\","
      ; "\"tests\": ["
      ; String.concat ",\n" (List.map Test_result.to_json results)
      ; "]"
      ; "}"
      ]
end

let error_msg =
  "There was an error grading your submission. Please verify that \
   `dune build` succeeds and `dune test` runs the given tests. It's \
   also possible that your submission breaks on a hidden test case, so \
   please double check your solution."

let error_json =
  String.concat "\n"
    [ "{"
    ; "\"score\": 0,"
    ; "\"stdout_visibility\": \"visible\","
    ; "\"output\": \"" ^ error_msg ^ "\""
    ; "}"
    ]

let get_info (path, status,_) =
  let rec test_labels = function
    | [OUnitTest.Label sublabel;_;OUnitTest.Label label;_;_] -> label, sublabel
    | _ :: l -> test_labels l
    | _ -> failwith "all tests need labels and sublabels"
  in
  let label, sublabel = test_labels path in
  (label, sublabel, status)

let run_tests_gradescope tests_and_max_scores =
  try
    let (tests, _) = List.split tests_and_max_scores in
    let test_infos = Test_info.test_infos tests_and_max_scores in
    let conf = !OUnitCore.run_test_tt_main_conf [] in
    let logger = OUnitLogger.null_logger in
    let runner = snd (OUnitRunner.choice conf) in
    let chooser = snd (OUnitChooser.choice conf) in
    let ounit_out =
      OUnitCore.run_test_tt
        conf
        logger
        runner
        chooser
        ("Gradescope Suite" >::: tests)
    in
    let statuses = List.map get_info ounit_out in
    print_string (Test_results.(to_json (of_statuses test_infos statuses)))
  with _ -> print_string error_json
let gs_test_case = test_case ~length:(Custom_length 1.)
