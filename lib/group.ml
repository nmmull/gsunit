include Group_intf
open Utils

module Meta = struct
  type t =
    {
      name: string option;
      max_score: float option;
    }

  let mk
      ?max_score
      ?name
      () =
    {
      name;
      max_score;
    }

  let name m = m.name
  let max_score m = m.max_score
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

let name_default t = Option.value (t |> meta |> name) ~default:"[unnamed group]"

type test = Test.test list t
type result = Test.result list t

let max_score_tests tests =
  let rec go (num, acc) = function
    | [] -> (num, acc)
    | test :: tests ->
       match Test.(test |> meta |> max_score) with
       | None -> go (num, acc) tests
       | Some max_score -> go (num + 1, max_score +. acc) tests
  in go (0, 0.) tests

let mk ?max_score ?name = mk (Meta.mk ?max_score ?name ())

let num_max_score_given t = fst (max_score_tests (value t))
let max_score_given t = snd (max_score_tests (value t))
let num_tests t = List.length (value t)
let num_remainder t = num_tests t - num_max_score_given t
let max_score_remainder t =
  let go max_score =
    (max_score -. max_score_given t)
    /. float_of_int (num_remainder t)
  in Option.map go (max_score (meta t))

let of_tests ?max_score ?name tests =
  let _ =
    Option.map
      (fun max_score ->
         if snd (max_score_tests tests) > max_score
         then raise InvalidGroupMaxScore
         else ())
      max_score
  in
  mk ?max_score ?name tests

let to_ounit_test tgroup =
  tgroup
  |> value
  |> List.map Test.to_ounit_test
  |> OUnit2.(>:::) (name_default tgroup)

let to_gradescope
      ?(group_name_formatter=default_group_name_formatter)
      ?output_formatter
      ?status_formatter
      rgroup =
  let name = rgroup |> meta |> name in
  let default_max_score = max_score_remainder rgroup in
  rgroup
  |> value
  |> List.map
       (Test.to_gradescope
          ?output_formatter
          ?status_formatter
          ?default_max_score
          ~group_name_formatter:(group_name_formatter name))

let test_to_result ounit_results =
  map
    (List.mapi
       (fun i ->
         Test.test_to_result
           (results_by_index i ounit_results)))
