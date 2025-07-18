open Utils

module Test_case = struct

  type status =
    | Passed
    | Failed

  (* type 'a with_options = *)
  (*   ?score:float -> *)
  (*   ?max_score:float -> *)
  (*   ?status:status -> *)
  (*   ?name:string -> *)
  (*   ?name_format:output_string_format -> *)
  (*   ?number:string -> *)
  (*   ?output:string -> *)
  (*   ?output_format:output_string_format -> *)
  (*   ?tags:string list -> *)
  (*   ?visibility:visibility -> *)
  (*   'a *)

  type t =
    {
      score: float option;
      max_score: float option;
      status: status option;
      name: string option;
      name_format: output_string_format;
      number: string option;
      output: string option;
      output_format: output_string_format;
      tags: string list;
      visibility: visibility;
    }

  let of_score
        ?max_score
        ?status
        ?name
        ?(name_format=Text)
        ?number
        ?output
        ?(output_format=Text)
        ?(tags=[])
        ?(visibility=Visible)
        score =
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
    }
end

module Test_suite = struct

  type t =
    {
      score: float option;
      execution_time: int option;
      output: string option;
      output_format: output_string_format;
      test_output_format: output_string_format;
      test_name_format: output_string_format;
      visibility: visibility;
      stdout_visibility: visibility;
      tests: Test_case.t list;
    }

  let of_tests
        ?score
        ?execution_time
        ?output
        ?(output_format=Text)
        ?(test_output_format=Text)
        ?(test_name_format=Text)
        ?(visibility=Visible)
        ?(stdout_visibility=Visible)
        tests =
    let invalid =
      Option.is_none score
      && List.exists
           Option.is_none
           (List.map
              (fun (t : Test_case.t) -> t.score)
              tests)
    in
    if not invalid
    then
      Some
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
        }
    else None
end
