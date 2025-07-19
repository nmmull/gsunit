type 'a with_options =
  ?hint:string ->
  ?length:float ->
  ?hidden:bool ->
  'a

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

type 'a t = Meta.t * 'a

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
  let open Meta in
  let meta = meta t in
  let length = OUnitTest.Custom_length (length meta) in
  let test_case = test_case ~length (value t) in
  (name meta) >: test_case
