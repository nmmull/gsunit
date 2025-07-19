type test_results = (SubTest.Meta.t * OUnitTest.result) list

type formatted_string = string * Gradescope.output_string_format
type output_formatter = test_results -> formatted_string option

let default_output_formatter _ = None
let text s = (s, `Text)
let html s = (s, `Html)
let simple_format s = (s, `Simple_format)
let md s = (s, `Md)
let ansi s = (s, `Ansi)

type status_checker = test_results -> Gradescope.status option
let default_status_checker _ = None

type ounit_test_runner = OUnitTest.(test -> result_list)

let default_ounit_test_runner : ounit_test_runner =
  let conf = !OUnitCore.run_test_tt_main_conf [] in
  let logger = OUnitLogger.null_logger in
  let runner = snd (OUnitRunner.choice conf) in
  let chooser = snd (OUnitChooser.choice conf) in
  OUnitCore.run_test_tt conf logger runner chooser

let results_by_index i =
  List.fold_left
    (fun acc (path, result) ->
      match path with
      | OUnitTest.Label _ :: OUnitTest.ListItem j :: path when i = j -> (path, result) :: acc
      | _ -> acc)
    []
