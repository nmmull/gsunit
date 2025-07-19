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

module type META = sig
  type t
  val max_score : t -> float option
  val name : t -> formatted_string
  val name_str : t -> string
  val name_format : t -> Gradescope.output_string_format
  val number : t -> string option
  val output : t -> formatted_string option
  val output_str : t -> string option
  val output_format : t -> Gradescope.output_string_format option
  val status : t -> Gradescope.status option
  val tags : t -> string list option
  val visibility : t -> Gradescope.visibility
  val extra_data : t -> Yojson.Basic.t option
  val hint : t -> string option
  val hidden : t -> bool
end

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
  let hint t = t.hint
  let hidden t = t.hidden
end

include (Meta : META with type t := Meta.t)

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
  ( Meta.mk
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
