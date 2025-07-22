open Utils

type output_string_format =
  [ `Text
  | `Html
  | `Simple_format
  | `Md
  | `Ansi
  ]

type status =
  [ `Passed
  | `Failed
  ]

type visibility =
  [ `Hidden
  | `After_due_date
  | `After_published
  | `Visible
  ]

module Test = struct

  type 'a with_options =
    ?score:float ->
    ?max_score:float ->
    ?status:status ->
    ?name:string ->
    ?name_format:output_string_format ->
    ?number:string ->
    ?output:string ->
    ?output_format:output_string_format ->
    ?tags:string list ->
    ?visibility:visibility ->
    ?extra_data:Yojson.Basic.t ->
    'a

  module type TEST = sig
    type t
    val score : t -> float option
    val max_score: t -> float option
    val status : t -> status option
    val name : t -> string option
    val name_format : t -> output_string_format option
    val number : t -> string option
    val output : t -> string option
    val output_format : t -> output_string_format option
    val tags : t -> string list option
    val visibility : t -> visibility option
    val extra_data : t -> Yojson.Basic.t option
  end

  type t =
    {
      score: float option;
      max_score: float option;
      status: status option;
      name: string option;
      name_format: output_string_format option;
      number: string option;
      output: string option;
      output_format: output_string_format option;
      tags: string list option;
      visibility: visibility option;
      extra_data: Yojson.Basic.t option;
    }

  let mk
        ?score
        ?max_score
        ?status
        ?name
        ?name_format
        ?number
        ?output
        ?output_format
        ?tags
        ?visibility
        ?extra_data
        () =
    {
      score;
      max_score;
      status;
      name;
      name_format;
      number;
      output;
      output_format;
      tags;
      visibility;
      extra_data;
    }

  let score t = t.score
  let max_score t = t.max_score
  let status t = t.status
  let name t = t.name
  let name_format t = t.name_format
  let number t = t.number
  let output t = t.output
  let output_format t = t.output_format
  let tags t = t.tags
  let visibility t = t.visibility
  let extra_data t = t.extra_data

  let to_json t =
    let assocs =
      Option.[
          "score", map json_of_float (score t);
          "max_score", map json_of_float (max_score t);
          "status", map json_of_status (status t);
          "name", map json_of_string (name t);
          "name_format", map json_of_format (name_format t);
          "number", map json_of_string (number t);
          "output", map json_of_string (output t);
          "output_format", map json_of_format (output_format t);
          "tags", map json_of_tags (tags t);
          "visibility", map json_of_visibility (visibility t);
          "extra_data", extra_data t;
      ]
    in
    `Assoc
      (List.filter_map
         (fun (id, opt_val) -> Option.map (fun v -> (id, v)) opt_val)
         assocs)
end

exception MissingTopLevelScore
exception MissingTestScore

type 'a with_options =
  ?score:float ->
  ?execution_time:int ->
  ?output:string ->
  ?output_format:output_string_format ->
  ?test_output_format:output_string_format ->
  ?test_name_format:output_string_format ->
  ?visibility:visibility ->
  ?stdout_visibility:visibility ->
  ?tests:Test.t list ->
  ?extra_data:Yojson.Basic.t ->
  'a

module type SUITE = sig
  type t
  val score : t -> float option
  val execution_time : t -> int option
  val output : t -> string option
  val output_format : t -> output_string_format option
  val test_output_format : t -> output_string_format option
  val test_name_format : t -> output_string_format option
  val visibility : t -> visibility option
  val stdout_visibility : t -> visibility option
  val tests : t -> Test.t list option
  val extra_data : t -> Yojson.Basic.t option
end


type t =
  {
    score: float option;
    execution_time: int option;
    output: string option;
    output_format: output_string_format option;
    test_output_format: output_string_format option;
    test_name_format: output_string_format option;
    visibility: visibility option;
    stdout_visibility: visibility option;
    tests: Test.t list option;
    extra_data: Yojson.Basic.t option;
  }

let mk
      ?score
      ?execution_time
      ?output
      ?output_format
      ?test_output_format
      ?test_name_format
      ?visibility
      ?stdout_visibility
      ?tests
      ?extra_data
      () =
  match score, tests with
  | None, None -> raise MissingTopLevelScore
  | None, Some tests
       when List.exists
              Option.is_none
              (List.map Test.score tests) ->
     raise MissingTestScore
  | _ ->
     {
       score;
       execution_time;
       output;
       output_format;
       test_output_format;
       test_name_format;
       visibility;
       stdout_visibility;
       tests;
       extra_data;
     }

let score t = t.score
let execution_time t = t.execution_time
let output t = t.output
let output_format t = t.output_format
let test_output_format t = t.test_output_format
let test_name_format t = t.test_name_format
let visibility t = t.visibility
let stdout_visibility t = t.stdout_visibility
let extra_data t = t.extra_data
let tests t = t.tests

let to_json t =
  let json_of_tests ts =
    `List (List.map Test.to_json ts)
  in
  let assocs =
    Option.[
        "score", map json_of_float (score t);
        "execution_time", map json_of_int (execution_time t);
        "output", map json_of_string (output t);
        "output_format", map json_of_format (output_format t);
        "test_output_format", map json_of_format (test_output_format t);
        "test_name_format", map json_of_format (test_name_format t);
        "visibility", map json_of_visibility (visibility t);
        "stdout_visibility", map json_of_visibility (stdout_visibility t);
        "extra_data", extra_data t;
        "tests", map json_of_tests (tests t);
    ]
  in
  `Assoc
    (List.filter_map
       (fun (id, opt_val) -> Option.map (fun v -> (id, v)) opt_val)
       assocs)
