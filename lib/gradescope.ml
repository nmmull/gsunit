type output_string_format =
  [ `Text
  | `Html
  | `Simple_format
  | `Md
  | `Ansi
  ]

let json_of_format = function
  | `Text -> `String "text"
  | `Html -> `String "html"
  | `Simple_format -> `String "simple_format"
  | `Md -> `String "md"
  | `Ansi -> `String "ansi"

type status =
  [ `Passed
  | `Failed
  ]

let json_of_status = function
  | `Passed -> `String "passed"
  | `Failed -> `String "failed"

type visibility =
  [ `Hidden
  | `After_due_date
  | `After_published
  | `Visible
  ]

let json_of_visibility = function
  | `Hidden -> `String "hidden"
  | `After_due_date -> `String "after_due_date"
  | `After_published -> `String "after_published"
  | `Visible -> `String "visible"

let json_of_float f = `Float f
let json_of_int n = `Int n
let json_of_string s = `String s
let json_of_tags t = `List (List.map json_of_string t)

module Test = struct

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
      extra_data: Yojson.Basic.t option
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
        map json_of_float (score t);
        map json_of_float (max_score t);
        map json_of_status (status t);
        map json_of_string (name t);
        map json_of_format (name_format t);
        map json_of_string (number t);
        map json_of_string (output t);
        map json_of_format (output_format t);
        map json_of_tags (tags t);
        map json_of_visibility (visibility t);
        (extra_data t);
      ]
    in
    `Assoc (List.filter_map Fun.id assocs)
end

module Suite = struct

  exception MissingTopLevelScore
  exception MissingTestScore

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
  let tests t = t.tests

  let to_json t =
    let json_of_tests ts =
      `List (List.map Test.to_json ts)
    in
    let assocs =
      Option.[
          map json_of_float (score t);
          map json_of_int (execution_time t);
          map json_of_string (output t);
          map json_of_format (output_format t);
          map json_of_format (test_output_format t);
          map json_of_format (test_name_format t);
          map json_of_visibility (visibility t);
          map json_of_visibility (stdout_visibility t);
          map json_of_tests (tests t);
      ]
    in `Assoc (List.filter_map Fun.id assocs)
end
