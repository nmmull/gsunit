include Test_intf
open Utils

module Meta = struct
  type t =
    {
      max_score: float option;
      name: string option;
      name_format: output_string_format;
      hint: string option;
      hidden: bool;
      number: string option;
      output: string option;
      output_format: output_string_format;
      status: status option;
      visibility: visibility;
      tags: string list option;
      extra_data: Yojson.Basic.t option;
    }

  let mk
        ?max_score
        ?hint
        ?(hidden=false)
        ?number
        ?output
        ?(output_format=`Text)
        ?status
        ?(visibility=`Visible)
        ?tags
        ?extra_data
        ?(name_format=`Text)
        ?name
        () =
    {
      max_score;
      name;
      name_format;
      hint;
      hidden;
      number;
      output;
      output_format;
      status;
      tags;
      visibility;
      extra_data;
    }

  let max_score t = t.max_score
  let name t = t.name
  let name_format t = t.name_format
  let number t = t.number
  let output t = t.output
  let output_format t = t.output_format
  let status t = t.status
  let tags t = t.tags
  let visibility t = t.visibility
  let extra_data t = t.extra_data
  let hint t = t.hint
  let hidden t = t.hidden
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

let name' m = Option.value (m |> name) ~default:"[unnamed test]"

type case =
  [ `Single of OUnitTest.test_fun
  | `Multi of SubTest.test list
  ]
type test = SubTest.test list t
type result = SubTest.result list t

let mk
      ?max_score
      ?hint
      ?hidden
      ?number
      ?output
      ?output_format
      ?status
      ?visibility
      ?tags
      ?extra_data
      ?name_format
      ?name =
  mk (Meta.mk
        ?hidden
        ?max_score
        ?hint
        ?number
        ?output
        ?output_format
        ?status
        ?tags
        ?visibility
        ?extra_data
        ?name_format
        ?name
        ())

let of_case
      ?max_score
      ?hint
      ?hidden
      ?number
      ?output
      ?output_format
      ?status
      ?visibility
      ?tags
      ?extra_data
      ?name_format
      ?name
      case =
  let case =
    match case with
    | `Single test_fun -> [ SubTest.mk test_fun ]
    | `Multi subtests -> subtests
  in
  mk
    ?hidden
    ?max_score
    ?hint
    ?number
    ?output
    ?output_format
    ?status
    ?tags
    ?visibility
    ?extra_data
    ?name_format
    ?name
    case

let update_meta
      ?max_score
      ?hint
      ?hidden
      ?number
      ?output
      ?output_format
      ?status
      ?visibility
      ?tags
      ?extra_data
      ?name_format
      ?name
      t =
  let m = t |> meta in
  let get x default =
    match x with
    | Some x -> Some x
    | None -> default
  in
  mk
    ?max_score:(get max_score (m |> Meta.max_score))
    ?hint:(get hint (m |> Meta.hint))
    ?hidden:(get hidden (Some (m |> Meta.hidden)))
    ?number:(get number (m |> Meta.number))
    ?output:(get output (m |> Meta.output))
    ?output_format:(get output_format (Some (m |> Meta.output_format)))
    ?status:(get status (m |> Meta.status))
    ?visibility:(get visibility (Some (m |> Meta.visibility)))
    ?tags:(get tags (m |> Meta.tags))
    ?extra_data:(get extra_data (m |> Meta.extra_data))
    ?name_format:(get name_format (Some (m |> Meta.name_format)))
    ?name:(get name (m |> Meta.name))
    (value t)

let to_ounit_test t =
  let open OUnit2 in
  let sub_tests = value t in
  sub_tests
  |> List.map SubTest.to_ounit_test
  |> (>:::) (t |> meta |> name')

let test_to_result ounit_results =
  map
    (List.mapi
       (fun i ->
         SubTest.map
           (fun _ ->
             List.assoc [i] ounit_results)))

type output_formatter = result -> formatted_string option
let default_output_formatter test_results =
  if test_results |> value |> List.length |> ((=) 1)
  then None
  else
    let failed r =
      r
      |> SubTest.value
      |> ((=) `Failed)
    in
    let subtest_label r =
      let open SubTest in
      if r |> meta |> hidden
      then "<hidden>"
      else Option.value (r |> meta |> hint) ~default:(r |> meta |> name')
    in
    let failed_list =
      test_results
      |> value
      |> List.mapi (fun i r -> (i, r))
      |> List.filter (fun (_, r) -> failed r)
      |> List.map (fun (i, r) -> (i, subtest_label r))
      |> List.map (fun (i, label) -> Printf.sprintf "%d: %s" i label)
    in
    let output_str =
      let lines =
        [
          "Failed Tests:";
          "-------------";
          "";
        ] @ failed_list
      in
      String.concat "\n" lines
    in
    if List.length failed_list = 0
    then None
    else Some (text output_str)

type status_formatter = result -> status option
let default_status_formatter _ = None

let num_sub_tests t  = List.length (value t)
let num_passed (t : result) =
  List.fold_left
    (fun acc r -> acc + (if (SubTest.value r) = `Passed then 1 else 0))
    0
    (value t)

let to_gradescope
      ?(output_formatter=default_output_formatter)
      ?(status_formatter=default_status_formatter)
      ~group_name_formatter
      ~default_max_score
      t =
  let t =
    let formatted_output = output_formatter t in
    update_meta
      ?output:(Option.map str formatted_output)
      ?output_format:(Option.map format formatted_output)
      ?status:(status_formatter t)
      t
  in
  let max_score =
    Option.value
      (t |> meta |> max_score)
      ~default:default_max_score
  in
  let max_score = floor3 max_score in
  let score =
    let num_sub_tests = num_sub_tests t in
    if num_sub_tests = 0
    then max_score
    else
      max_score
      *. float_of_int (num_passed t)
      /. float_of_int num_sub_tests
  in
  let score = ceil3 score in
  let name =
    if (t |> meta |> hidden)
    then text "<hidden>"
    else
      match t |> meta |> hint with
      | Some hint -> text hint
      | None ->
         group_name_formatter
           (format_str (t |> meta |> name') (t |> meta |> name_format))
  in
  Gradescope.Test.mk
    ?visibility:(t |> meta |> visibility |> opt_of_visibility)
    ?status:(t |> meta |> status)
    ?output:(t |> meta |> output)
    ?output_format:(t |> meta |> output_format |> opt_of_format)
    ?tags:(t |> meta |> tags)
    ?extra_data:(t |> meta |> extra_data)
    ?number:(t |> meta |> number)
    ?name_format:(format name |> opt_of_format)
    ~max_score
    ~score
    ~name:(str name)
    ()
