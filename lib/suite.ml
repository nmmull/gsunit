open Utils

type 'a with_options =
  ?output:Formatted_string.t ->
  ?visibility:Gradescope.visibility ->
  ?stdout_visibility:Gradescope.visibility ->
  ?extra_data:Yojson.Basic.t ->
  'a

module type META = sig
  type t
  val output : t -> Formatted_string.t option
  val output_str : t -> string option
  val output_format : t -> Gradescope.output_string_format option
  val visibility : t -> Gradescope.visibility
  val stdout_visibility : t -> Gradescope.visibility
  val extra_data : t -> Yojson.Basic.t option
end

module Meta = struct
  type t =
    {
      output: Formatted_string.t option;
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

include (Meta : META with type t := Meta.t)

type 'a t = Meta.t * 'a
type test = Group.test list t
type result = Group.result list t

let mk
      ?output
      ?visibility
      ?stdout_visibility
      ?extra_data
      a =
  ( Meta.mk
      ?output
      ?visibility
      ?stdout_visibility
      ?extra_data
      ()
  , a
  )

let meta (m, _) = m
let value (_, t) = t

let to_ounit_test suite =
  suite
  |> value
  |> List.map Group.to_ounit_test
  |> OUnit2.(>:::) "Gradescope suite"

let to_gradescope suite =
  Gradescope.Suite.mk
    ?output:(suite |> meta |> output_str)
    ?output_format:(suite |> meta |> output_format)
    ?visibility:(suite |> meta |> visibility |> opt_of_visibility)
    ?stdout_visibility:(suite |> meta |> stdout_visibility |> opt_of_visibility)
    ?extra_data:(suite |> meta |> extra_data)
    ~tests:(suite |> value |> List.concat_map Group.to_gradescope)
    ()

  (* let result_list = *)
  (*   suite *)
  (*   |> to_ounit_test *)
  (*   |> ounit_test_runner *)
  (*   |> List.map (fun (path, result, _) -> List.rev path, result) *)
  (* in *)
  (* let tests =
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
   *)
