open Utils

type 'a with_options =
  ?max_score:float ->
  ?hint:string ->
  ?hidden:bool ->
  ?number:string ->
  ?output: formatted_string ->
  ?status: Gradescope.status ->
  ?visibility:Gradescope.visibility ->
  ?tags:string list ->
  ?extra_data: Yojson.Basic.t ->
  'a

module Meta = struct
  type t =
    {
      max_score: float option;
      name: formatted_string;
      hint: string option;
      hidden: bool;
      number: string option;
      output: formatted_string option;
      status: Gradescope.status option;
      visibility: Gradescope.visibility;
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
        name =
    {
      max_score;
      name;
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
  let name_str t = fst t.name
  let name_format t = snd t.name
  let number t = t.number
  let output t = t.output
  let output_str t = Option.map fst (output t)
  let output_format t = Option.map snd (output t)
  let status t = t.status
  let tags t = t.tags
  let visibility t = t.visibility
  let extra_data t = t.extra_data
end

include Meta

type 'a t = Meta.t * 'a
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
      name
      a =
  ( mk
      ?hidden
      ?max_score
      ?hint
      ?number
      ?output
      ?status
      ?tags
      ?visibility
      ?extra_data
      name
  , a
  )

let of_test_fun
      ?max_score
      ?hint
      ?hidden
      ?number
      ?output
      ?status
      ?visibility
      ?tags
      ?extra_data
      name
      test_fun =
  mk
    ?hidden
    ?max_score
    ?hint
    ?number
    ?output
    ?status
    ?tags
    ?visibility
    ?extra_data
    name
    [ SubTest.mk (fst name) test_fun ]

let meta (m, _) = m
let value (_, a) = a

let to_ounit_test t =
  let open OUnit2 in
  let sub_tests = value t in
  sub_tests
  |> List.map SubTest.to_ounit_test
  |> (>:::) (t |> meta |> name_str)

let num_sub_tests t  = List.length (value t)
let num_passed (t : result) =
  List.fold_left
    (fun acc r -> acc + (if (SubTest.value r) = OUnitTest.RSuccess then 1 else 0))
    0
    (value t)

let to_gradescope
      group_name
      default_max_score
      (t : result) =
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
  let output_format =
    match t |> meta |> output_format with
    | Some `Text -> None
    | f -> f
  in
  let visibility =
    match t |> meta |> visibility with
    | `Visible -> None
    | v -> Some v
  in
  let name =
    let name_str = t |> meta |> name_str in
    if num_sub_tests t = 1
       && t |> meta |> name_format = `Text
    then "[" ^ group_name ^ "] " ^ name_str
    else name_str
  in
  let name_format =
    match t |> meta |> name_format with
    | `Text -> None
    | f -> Some f
  in
  Gradescope.Test.mk
    ?output_format
    ?visibility
    ?name_format
    ?status:(t |> meta |> status)
    ?output:(t |> meta |> output_str)
    ?tags:(t |> meta |> tags)
    ?extra_data:(t |> meta |> extra_data)
    ?number:(t |> meta |> number)
    ~name
    ~max_score
    ~score
    ()
