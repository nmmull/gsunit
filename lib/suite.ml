open Utils

module Meta = struct
  type t =
    {
      output: formatted_string option;
      visibility: Gradescope.visibility;
      stdout_visibility: Gradescope.visibility;
      extra_data: Yojson.Basic.t option
    }

  let mk
        ?output
        ?(visibility=`Visible)
        ?(stdout_visibility=`Visible)
        ?extra_data
        () =
    {
      output;
      visibility;
      stdout_visibility;
      extra_data;
    }

  let output m = m.output
  let output_str m = Option.map fst (output m)
  let output_format m = Option.map snd (output m)
  let visibility m = m.visibility
  let stdout_visibility m = m.stdout_visibility
  let extra_data m = m.extra_data
end

type t = Meta.t * Group.t list

let mk
      ?output
      ?visibility
      ?stdout_visibility
      ?extra_data
      tests =
  ( Meta.mk
      ?output
      ?visibility
      ?stdout_visibility
      ?extra_data
      ()
  , tests
  )

let output (m, _) = Meta.output m
let output_str (m, _) = Meta.output_str m
let output_format (m, _) = Meta.output_format m
let visibility (m, _) = Meta.visibility m
let stdout_visibility (m, _) = Meta.stdout_visibility m
let extra_data (m, _) = Meta.extra_data m
let groups (_, t) = t

let to_ounit_test suite =
  suite
  |> groups
  |> List.map Group.to_ounit_test
  |> OUnit2.(>:::) "Gradescope suite"

let to_gradescope
      ?(ounit_test_runner=default_ounit_test_runner)
      suite =
  let result_list =
    suite
    |> to_ounit_test
    |> ounit_test_runner
    |> List.map (fun (path, result, _) -> List.rev path, result)
  in
  let tests =
    List.concat
      (List.mapi
         (fun i g ->
           Group.to_gradescope
             (Utils.results_by_index i result_list)
             g)
         (groups suite))
  in
  let visibility =
    match visibility suite with
    | `Visible -> None
    | v -> Some v
  in
  let stdout_visibility =
    match stdout_visibility suite with
    | `Visible -> None
    | v -> Some v
  in
  Gradescope.Suite.mk
    ?output:(output_str suite)
    ?output_format:(output_format suite)
    ?visibility
    ?stdout_visibility
    ?extra_data:(extra_data suite)
    ~tests
