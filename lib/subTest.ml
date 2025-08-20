include SubTest_intf
open Utils

module Meta = struct
  type t =
    {
      name: string option;
      length: float;
    }

  let mk ?name ?(length=2.0) () = {name; length}

  let name m = m.name
  let length m = m.length
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

let name' m = Option.value (name m) ~default:"[unnamed subtest]"

let mk
      ?name
      ?length =
  mk (Meta.mk
        ?name
        ?length
        ())

type test = OUnitTest.test_fun t
type result = [ `Passed | `Failed ] t

type result_formatter = result list -> formatted_string option

let of_test_fun ?name ?length = mk ?name ?length

let to_ounit_test t =
  let open OUnit2 in
  let length = OUnitTest.Custom_length (t |> meta |> length) in
  let test_case = test_case ~length (value t) in
  (t |> meta |> name') >: test_case
