(* type ounit_results = (OUnitTest.node list * OUnitTest.result) list *)

type ounit_results = (int list * [ `Passed | `Failed ]) list

type ounit_test_runner = OUnitTest.(test -> result_list)

let default_ounit_test_runner : ounit_test_runner =
  let conf = !OUnitCore.run_test_tt_main_conf [] in
  let logger = OUnitLogger.null_logger in
  let runner = snd (OUnitRunner.choice conf) in
  let chooser = snd (OUnitChooser.choice conf) in
  OUnitCore.run_test_tt conf logger runner chooser

let reformat_ounit_results results =
  let reformat_path =
    let rec go acc = function
      | [] -> List.rev acc
      | OUnitTest.ListItem i :: path -> go (i :: acc) path
      | _ :: path -> go acc path
    in go []
  in
  let reformat_result = function
    | OUnitTest.RSuccess -> `Passed
    | _ -> `Failed
  in
  List.map
    (fun (path, result, _) ->
      reformat_path path, reformat_result result)
    results

let results_by_index i =
  List.fold_left
    (fun acc (path, result) ->
      match path with
      | j :: path when i = j -> (path, result) :: acc
      | _ -> acc)
    []

let opt_of_visibility = function
  | `Visible -> None
  | v -> Some v

let opt_of_format = function
  | `Text -> None
  | f -> Some f

let json_of_format = function
  | `Text -> `String "text"
  | `Html -> `String "html"
  | `Simple_format -> `String "simple_format"
  | `Md -> `String "md"
  | `Ansi -> `String "ansi"

let json_of_status = function
  | `Passed -> `String "passed"
  | `Failed -> `String "failed"

let json_of_visibility = function
  | `Hidden -> `String "hidden"
  | `After_due_date -> `String "after_due_date"
  | `After_published -> `String "after_published"
  | `Visible -> `String "visible"

let json_of_float f = `Float f
let json_of_int n = `Int n
let json_of_string s = `String s
let json_of_tags t = `List (List.map json_of_string t)
