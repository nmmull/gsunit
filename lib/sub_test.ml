type t =
  {
    name: string option;
    hint: string option;
    length: float;
    hidden: bool;
    test_fun: OUnitTest.test_fun;
  }

let of_test_fun
      ?name
      ?hint
      ?(length=1.0)
      ?(hidden=false)
      test_fun =
  {
    name;
    hint;
    hidden;
    length;
    test_fun;
  }

let to_ounit_test_case sub_test =
  let open OUnit2 in
  let length = OUnitTest.Custom_length sub_test.length in
  let test_case = test_case ~length sub_test.test_fun in
  match sub_test.name with
  | None -> test_case
  | Some name -> name >: test_case
