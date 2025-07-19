open Utils

type 'a with_options =
  ?output:formatted_string ->
  ?visibility:Gradescope.visibility ->
  ?stdout_visibility:Gradescope.visibility ->
  ?extra_data:Yojson.Basic.t ->
  'a

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

let meta (m, _) = m
let groups (_, t) = t

let output t = Meta.output (meta t)
let output_str t = Meta.output_str (meta t)
let output_format t = Meta.output_format (meta t)
let visibility t = Meta.visibility (meta t)
let stdout_visibility t = Meta.stdout_visibility (meta t)
let extra_data t = Meta.extra_data (meta t)

let to_ounit_test suite =
  suite
  |> groups
  |> List.map Group.to_ounit_test
  |> OUnit2.(>:::) "Gradescope suite"

let to_gradescope
      ounit_results
      suite =
  (* let result_list = *)
  (*   suite *)
  (*   |> to_ounit_test *)
  (*   |> ounit_test_runner *)
  (*   |> List.map (fun (path, result, _) -> List.rev path, result) *)
  (* in *)
  let tests =
    List.concat
      (List.mapi
         (fun i g ->
           Group.to_gradescope
             (Utils.results_by_index i ounit_results)
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
