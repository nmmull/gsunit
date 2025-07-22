include SubTest_intf
open Utils

module Meta = struct
  type t =
    {
      name: string;
      hint: string option;
      hidden: bool;
      length: float;
    }

  let mk
        ?hint
        ?(length=2.0)
        ?(hidden=false)
        name =
    {name; hint; length; hidden}

  let name m = m.name
  let hint m = m.hint
  let length m = m.length
  let hidden m = m.hidden
end

include (Meta : META with type t := Meta.t)
include With_meta (Meta)

let mk
      ?hint
      ?length
      ?hidden
      name =
  mk (Meta.mk
        ?hint
        ?length
        ?hidden
        name)

type case = OUnitTest.test_fun
type test = case t
type result = [ `Passed | `Failed ] t

let to_ounit_test t =
  let open OUnit2 in
  let length = OUnitTest.Custom_length (t |> meta |> length) in
  let test_case = test_case ~length (value t) in
  (t |> meta |> name) >: test_case
