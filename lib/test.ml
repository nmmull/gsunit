open Utils

exception EmptySubTests

module Meta = struct
  type t =
    {
      max_score: float option;
      name: string option;
      hint: string option;
      hidden: bool;
      name_format: output_string_format;
      number: string option;
      output_formatter : output_formatter;
      tags: string list;
      visibility: visibility;
    }

  let mk
        ~max_score
        ~name
        ~hint
        ~hidden
        ~name_format
        ~number
        ~output_formatter
        ~tags
        ~visibility =
    {
      max_score;
      name;
      hint;
      hidden;
      name_format;
      number;
      output_formatter;
      tags;
      visibility;
    }

  let max_score t = t.max_score
  let name t =
    if t.hidden
    then
      Some (Option.value t.hint ~default:"[hidden]")
    else t.name
  let name_format t = t.name_format
  let number t = t.number
  let output_formatter t = t.output_formatter
  let tags t = t.tags
  let visibility t = t.visibility
end

type t = Meta.t * SubTest.t list

let of_test_fun
      ?max_score
      ?name
      ?hint
      ?(hidden=false)
      ?(name_format=Text)
      ?number
      ?(output_formatter=default_output_formatter)
      ?(tags=[])
      ?(visibility=Visible)
      test_fun =
  ( Meta.mk
      ~max_score
      ~name
      ~hint
      ~hidden
      ~name_format
      ~number
      ~output_formatter
      ~tags
      ~visibility
  , [SubTest.mk test_fun]
  )

let of_sub_tests
      ?max_score
      ?name
      ?hint
      ?(hidden=false)
      ?(name_format=Text)
      ?number
      ?(output_formatter=default_output_formatter)
      ?(tags=[])
      ?(visibility=Visible)
      sub_tests =
  if List.is_empty sub_tests
  then raise EmptySubTests
  else
    ( Meta.mk
        ~max_score
        ~name
        ~hint
        ~hidden
        ~name_format
        ~number
        ~output_formatter
        ~tags
        ~visibility
    , sub_tests
  )

let max_score (m, _) = Meta.max_score m
let name (m, _) = Meta.name m
let name_format (m, _) = Meta.name m
let number (m, _) = Meta.number m
let output_formatter (m, _) = Meta.output_formatter m
let tags (m, _) = Meta.tags m
let visibility (m, _) = Meta.visibility m
let sub_tests (_, t) = t

let to_ounit_test t =
  let open OUnit2 in
  let ounit_test =
    t
    |> sub_tests
    |> List.map SubTest.to_ounit_test
    |> test_list
  in
  match name t with
  | None -> ounit_test
  | Some name -> name >: ounit_test
