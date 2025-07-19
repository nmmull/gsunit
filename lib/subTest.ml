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

type t = Meta.t * OUnitTest.test_fun

let mk
      ?hint
      ?length
      ?hidden
      name
      test_fun =
  ( Meta.mk
      ?hint
      ?length
      ?hidden
      name
  , test_fun
  )

let meta (m, _) = m
let name (m, _) = Meta.name m
let hint (m, _) = Meta.hint m
let length (m, _) = Meta.length m
let hidden (m, _) = Meta.hidden m
let test_fun (_, t) = t

let to_ounit_test t =
  let open OUnit2 in
  let length = OUnitTest.Custom_length (length t) in
  let test_case = test_case ~length (test_fun t) in
  (name t) >: test_case
