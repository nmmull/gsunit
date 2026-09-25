include SubTest_intf
open Utils

module Meta = struct
  type t =
    {
      name: string option;
    }

  let mk ?name () = {name; }

  let name m = m.name
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

let name' m = Option.value (name m) ~default:"[unnamed subtest]"

let mk ?name = mk (Meta.mk ?name ())

type test = OUnitTest.test_fun t
type result = [ `Passed | `Failed ] t

type result_formatter = result list -> formatted_string option

let of_test_fun ?name = mk ?name

let to_ounit_test t =
  let open OUnit2 in
  let test_case = test_case (value t) in
  (t |> meta |> name') >: test_case
