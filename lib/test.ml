include Test_intf
open Utils

module Meta = struct
  type t =
    {
      max_score: float option;
      name: formatted_string;
      hint: string option;
      hidden: bool;
      number: string option;
      output: formatted_string option;
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
        ?status
        ?(visibility=`Visible)
        ?tags
        ?extra_data
        ?(name_format=`Text)
        ~name
        () =
    {
      max_score;
      name=(name, name_format); (* TODO: FIX *)
      hint;
      hidden;
      number;
      output;
      status;
      tags;
      visibility;
      extra_data;
    }

  let max_score t = t.max_score
  let name t = t.name
  let name_str t = str t.name
  let name_format t = format t.name
  let number t = t.number
  let output t = t.output
  let output_str t = Option.map str (output t)
  let output_format t = Option.map format (output t)
  let status t = t.status
  let tags t = t.tags
  let visibility t = t.visibility
  let extra_data t = t.extra_data
  let hint t = t.hint
  let hidden t = t.hidden
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

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
      ?status
      ?visibility
      ?tags
      ?extra_data
      ?name_format
      ~name
      case =
  let case =
    match case with
    | `Single test_fun -> [ SubTest.mk ~name:"dummy" test_fun ]
    | `Multi subtests -> subtests
  in
  mk (Meta.mk
        ?hidden
        ?max_score
        ?hint
        ?number
        ?output
        ?status
        ?tags
        ?visibility
        ?extra_data
        ?name_format
        ~name
        ())
    case

let to_ounit_test t =
  let open OUnit2 in
  let sub_tests = value t in
  sub_tests
  |> List.map SubTest.to_ounit_test
  |> (>:::) (t |> meta |> name_str)

let num_sub_tests t  = List.length (value t)
let num_passed (t : result) =
  List.fold_left
    (fun acc r -> acc + (if (SubTest.value r) = `Passed then 1 else 0))
    0
    (value t)

let to_gradescope group_name default_max_score t =
  let max_score =
    Option.value
      (t |> meta |> max_score)
      ~default:default_max_score
  in
  let score =
    let num_sub_tests = num_sub_tests t in
    if num_sub_tests = 0
    then max_score
    else
      max_score
      *. float_of_int (num_passed t)
      /. float_of_int num_sub_tests
  in
  let name =
    let name_str = t |> meta |> name_str in
    if num_sub_tests t = 1
       && t |> meta |> name_format = `Text
    then "[" ^ group_name ^ "] " ^ name_str
    else name_str
  in
  Gradescope.Test.mk
    ?visibility:(t |> meta |> visibility |> opt_of_visibility)
    ?status:(t |> meta |> status)
    ?output:(t |> meta |> output_str)
    ?output_format:Option.(t |> meta |> output_format |> map opt_of_format |> join)
    ?tags:(t |> meta |> tags)
    ?extra_data:(t |> meta |> extra_data)
    ?number:(t |> meta |> number)
    ?name_format:(t |> meta |> name_format |> opt_of_format)
    ~max_score
    ~score
    ~name
    ()

let test_to_result ounit_results =
  map
    (List.mapi
       (fun i ->
         SubTest.map
           (fun _ ->
             List.assoc [i] ounit_results)))
