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
let map f (m, a) = (m, f a)

let to_ounit_test suite =
  suite
  |> value
  |> List.map Group.to_ounit_test
  |> OUnit2.(>:::) "Gradescope suite"

let to_gradescope suite =
  Gradescope.mk
    ?output:(suite |> meta |> output_str)
    ?output_format:(suite |> meta |> output_format)
    ?visibility:(suite |> meta |> visibility |> opt_of_visibility)
    ?stdout_visibility:(suite |> meta |> stdout_visibility |> opt_of_visibility)
    ?extra_data:(suite |> meta |> extra_data)
    ~tests:(suite |> value |> List.concat_map Group.to_gradescope)
    ()

let test_to_result ounit_results =
  map
    (List.mapi
       (fun i ->
         (Group.test_to_result
            (results_by_index i ounit_results))))
