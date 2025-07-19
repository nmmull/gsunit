type 'a with_options =
  ?hint:string ->
  ?length:float ->
  ?hidden:bool ->
  'a

module type META = sig
  type t
  val name : t -> string
  val hint : t -> string option
  val length : t -> float
  val hidden : t -> bool
end

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

type 'a t = Meta.t * 'a
type test = OUnitTest.test_fun t
type result = OUnitTest.result t

let mk
      ?hint
      ?length
      ?hidden
      name
      a =
  ( Meta.mk
      ?hint
      ?length
      ?hidden
      name
  , a
  )

let meta (m, _) = m
let value (_, a) = a

let to_ounit_test t =
  let open OUnit2 in
  let length = OUnitTest.Custom_length (t |> meta |> length) in
  let test_case = test_case ~length (value t) in
  (t |> meta |> name) >: test_case
