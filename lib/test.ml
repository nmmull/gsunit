open Utils

type 'a with_options =
  ?max_score:float ->
  ?hint:string ->
  ?hidden:bool ->
  ?number:string ->
  ?output_formatter:Utils.output_formatter ->
  ?status_checker:Utils.status_checker ->
  ?visibility:Gradescope.visibility ->
  ?tags:string list ->
  ?extra_data: Yojson.Basic.t ->
  'a

module Meta = struct
  type t =
    {
      max_score: float option;
      name: formatted_string;
      hint: string option;
      hidden: bool;
      number: string option;
      output_formatter: output_formatter;
      status_checker: status_checker;
      visibility: Gradescope.visibility;
      tags: string list option;
      extra_data: Yojson.Basic.t option;
    }

  let mk
        ?max_score
        ?hint
        ?(hidden=false)
        ?number
        ?(output_formatter=default_output_formatter)
        ?(status_checker=default_status_checker)
        ?(visibility=`Visible)
        ?tags
        ?extra_data
        name =
    {
      max_score;
      name;
      hint;
      hidden;
      number;
      output_formatter;
      status_checker;
      tags;
      visibility;
      extra_data;
    }

  let max_score t = t.max_score
  let name t = t.name
  let name_str t = fst t.name
  let name_format t = snd t.name
  let number t = t.number
  let output_formatter t = t.output_formatter
  let status_checker t = t.status_checker
  let tags t = t.tags
  let visibility t = t.visibility
  let extra_data t = t.extra_data
end

include Meta

type 'a t = Meta.t * 'a
type test = SubTest.test list t
type result = SubTest.result list t

let of_sub_tests
      ?max_score
      ?hint
      ?hidden
      ?number
      ?output_formatter
      ?status_checker
      ?visibility
      ?tags
      ?extra_data
      name
      sub_tests =
  ( Meta.mk
      ?hidden
      ?max_score
      ?hint
      ?number
      ?output_formatter
      ?status_checker
      ?tags
      ?visibility
      ?extra_data
      name
  , sub_tests
  )

let of_test_fun
      ?max_score
      ?hint
      ?hidden
      ?number
      ?output_formatter
      ?status_checker
      ?visibility
      ?tags
      ?extra_data
      name
      test_fun =
  ( Meta.mk
      ?max_score
      ?hint
      ?hidden
      ?number
      ?output_formatter
      ?status_checker
      ?tags
      ?visibility
      ?extra_data
      name
  , [SubTest.mk (fst name) test_fun]
  )

let meta (m, _) = m
let value (_, a) = a
(* let max_score (m, _) = Meta.max_score m *)
(* let name (m, _) = Meta.name m *)
(* let name_str (m, _) = Meta.name_str m *)
(* let name_format (m, _) = Meta.name_format m *)
(* let number (m, _) = Meta.number m *)
(* let output_formatter (m, _) = Meta.output_formatter m *)
(* let status_checker (m, _) = Meta.status_checker m *)
(* let tags (m, _) = Meta.tags m *)
(* let visibility (m, _) = Meta.visibility m *)
(* let extra_data (m, _) = Meta.extra_data m *)
(* let sub_tests (_, t) = t *)

let to_ounit_test t =
  let open OUnit2 in
  let meta = meta t in
  meta
  |> sub_tests
  |> List.map SubTest.to_ounit_test
  |> (>:::) (name_str meta)

let to_gradescope
      ?group_name
      default_max_score
      test_results
      t =
  let status = status_checker t test_results in
  let max_score = Option.value (max_score t) ~default:default_max_score in
  let score =
    let num_total = List.length test_results in
    let num_passed =
      List.fold_left
        (fun acc (_, r) -> acc + (if r = OUnitTest.RSuccess then 1 else 0))
        0
        test_results
    in
    if num_total = 0
    then max_score
    else
      max_score
      *. float_of_int num_passed
      /. float_of_int num_total
  in
  let (output, output_format) =
    match output_formatter t test_results with
    | Some (output, output_format) ->
       let output_format =
         match output_format with
         | `Text -> None
         | _ -> Some output_format
       in
       (Some output, output_format)
    | None -> (None, None)
  in
  let visibility =
    match visibility t with
    | `Visible -> None
    | v -> Some v
  in
  let name =
    match group_name, name_format t with
    | Some group_name, `Text -> "[" ^ group_name ^ "] " ^ name_str t
    | _ -> name_str t
  in
  let name_format =
    match name_format t with
    | `Text -> None
    | f -> Some f
  in
  Gradescope.Test.mk
    ?status
    ?output
    ?output_format
    ?visibility
    ?name_format
    ?tags:(tags t)
    ?extra_data:(extra_data t)
    ?number:(number t)
    ~name
    ~max_score
    ~score
    ()
