include Suite_intf
open Utils

module Meta = struct
  type t =
    {
      output: string option;
      output_format: output_string_format;
      visibility: visibility;
      stdout_visibility: visibility;
      extra_data: Yojson.Basic.t option
    }

  let mk
      ?output
      ?(output_format=`Simple_format)
      ?(visibility=`Visible)
      ?(stdout_visibility=`Visible)
      ?extra_data
      () =
    {
      output;
      output_format;
      visibility;
      stdout_visibility;
      extra_data;
    }

  let output m = m.output
  let output_format m = m.output_format
  let visibility m = m.visibility
  let stdout_visibility m = m.stdout_visibility
  let extra_data m = m.extra_data
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

type test = Group.test list t
type result = Group.result list t

let mk
    ?output
    ?output_format
    ?visibility
    ?stdout_visibility
    ?extra_data =
  mk (Meta.mk
        ?output
        ?output_format
        ?visibility
        ?stdout_visibility
        ?extra_data
        ())

let to_ounit_test suite =
  suite
  |> value
  |> List.map Group.to_ounit_test
  |> OUnit2.(>:::) "Gradescope suite"

let to_gradescope
      ?group_name_formatter
      ?output_formatter
      ?status_formatter
      suite =
  let to_gradescope r =
    Group.to_gradescope
      ?group_name_formatter
      ?output_formatter
      ?status_formatter
      r
  in
  Gradescope.Suite.mk
    ?output:(suite |> meta |> output)
    ~output_format:(suite |> meta |> output_format)
    ?visibility:(suite |> meta |> visibility |> opt_of_visibility)
    ?stdout_visibility:(suite |> meta |> stdout_visibility |> opt_of_visibility)
    ?extra_data:(suite |> meta |> extra_data)
    ~tests:(suite |> value |> List.concat_map to_gradescope)
    ()

let test_to_result ounit_results =
  map
    (List.mapi
       (fun i ->
         (Group.test_to_result
            (results_by_index i ounit_results))))
