open Utils

module Meta = struct
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
    }

  let mk
        ~score
        ~execution_time
        ~output
        ~output_format
        ~test_output_format
        ~test_name_format
        ~visibility
        ~stdout_visibility =
    {
      score;
      execution_time;
      output;
      output_format;
      test_output_format;
      test_name_format;
      visibility;
      stdout_visibility;
    }

  let score m = m.score
  let execution_time m = m.execution_time
  let output m = m.output
  let output_format m = m.output_format
  let test_output_format m = m.test_output_format
  let test_name_format m = m.test_name_format
  let visibility m = m.visibility
  let stdout_visibility m = m.stdout_visibility
end

type test =
  | Single of Test.t
  | Group of Group.t

type t = Meta.t * test list

let mk
      ?score
      ?execution_time
      ?output
      ?(output_format=Text)
      ?(test_output_format=Text)
      ?(test_name_format=Text)
      ?(visibility=Visible)
      ?(stdout_visibility=Visible)
      tests =
  ( Meta.mk
      ~score
      ~execution_time
      ~output
      ~output_format
      ~test_output_format
      ~test_name_format
      ~visibility
      ~stdout_visibility
  , tests
  )

let score (m, _) = Meta.score m
let execution_time (m, _) = Meta.execution_time m
let output (m, _) = Meta.output m
let output_format (m, _) = Meta.output_format m
let test_output_format (m, _) = Meta.test_output_format m
let test_name_format (m, _) = Meta.test_name_format m
let visibility (m, _) = Meta.visibility m
let stdout_visibility (m, _) = Meta.stdout_visibility m
let tests (_, t) = t

let to_ounit_test suite =
  let test_to_ounit_test = function
    | Single t -> Test.to_ounit_test t
    | Group g ->
       g
       |> Group.tests
       |> List.map Test.to_ounit_test
       |> OUnit2.test_list
       |> OUnit2.(>:) (Group.name g)
  in
  suite
  |> tests
  |> List.map test_to_ounit_test
  |> OUnit2.test_list

let to_gs_spec _ounit_runner =
  (* run OUnit2 on generated unit tests *)
  (* use results to construct test_case objects *)
  (* use the flatten groups *)
  (* construct full spec *)
  assert false
