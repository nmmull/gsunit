module Meta = struct
  type t =
    {
      name: string option;
      hint: string option;
      length: float;
      hidden: bool;
    }

  let mk
        ~name
        ~hint
        ~length
        ~hidden =
    {name; hint; length; hidden}

  let name m = m.name
  let hint m = m.hint
  let length m = m.length
  let hidden m = m.hidden
end

type t = Meta.t * OUnitTest.test_fun

let mk
      ?name
      ?hint
      ?(length=1.0)
      ?(hidden=false)
      test_fun =
  Meta.mk ~name ~hint ~length ~hidden, test_fun

let name (m, _) = Meta.name m
let hint (m, _) = Meta.hint m
let length (m, _) = Meta.length m
let hidden (m, _) = Meta.hidden m
let test_fun (_, t) = t

let to_ounit_test t =
  let open OUnit2 in
  let length = OUnitTest.Custom_length (length t) in
  let test_case = test_case ~length (test_fun t) in
  match name t with
  | None -> test_case
  | Some name -> name >: test_case
