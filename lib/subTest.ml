include SubTest_intf
open Utils

module Meta = struct
  type t =
    {
      name: string option;
      hint: string option;
      hidden: bool;
      length: float;
    }

  let mk
        ?name
        ?hint
        ?(length=2.0)
        ?(hidden=false)
        () =
    {name; hint; length; hidden}

  let name m = m.name
  let hint m = m.hint
  let length m = m.length
  let hidden m = m.hidden
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

let name' m = Option.value (m |> name) ~default:"[unnamed subtest]"

let mk
      ?name
      ?hint
      ?length
      ?hidden =
  mk (Meta.mk
        ?name
        ?hint
        ?length
        ?hidden
        ())

type test = OUnitTest.test_fun t
type result = [ `Passed | `Failed ] t

let of_test_fun
      ?name
      ?hint
      ?length
      ?hidden =
  mk ?name ?hint ?length ?hidden

let to_ounit_test t =
  let open OUnit2 in
  let length = OUnitTest.Custom_length (t |> meta |> length) in
  let test_case = test_case ~length (value t) in
  (t |> meta |> name') >: test_case
