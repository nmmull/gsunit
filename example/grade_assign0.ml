
module Sqrt = struct
  let name = "sqrt"
  let max_score = 5.0
  let sqrt = Assign0.sqrt

  let tests =
    let t =
      Gsunit.check
        ~pp_in:Fmt.int
        ~pp_out:Fmt.int
        sqrt "sqrt"
    in
    Gsunit.group
      ~name
      ~max_score
      [
        t 4 2;
        t 9 3;
        t 100 10;
        t 2 2;
        t 10 4;
        t 99 10;
      ]
end

module Is_prime = struct
  let name = "is_prime"
  let max_score = 5.
  let is_prime = Assign0.is_prime

  let is_prime_ref (n : int) : bool =
    let rec loop (i : int) : bool =
      if i >= n
      then true
      else if n mod i = 0
      then false
      else loop (i + 1)
    in
    if n < 2
    then false
    else loop 2

  let test =
    let t =
      Gsunit.check_sub_ref
        ~pp_in:Fmt.int
        ~pp_out:Fmt.bool
        is_prime
        "is_prime"
        is_prime_ref
    in
    Gsunit.test
      ~name:"simple tests"
      (`Multi
         [
           t 0;
           t 1;
           t 2;
           t 37;
           t 57;
           t 97;
         ])

  let other_tests =
    let t =
      Gsunit.check_sub_ref
        ~pp_in:Fmt.int
        ~pp_out:Fmt.bool
        is_prime
        "is_prime"
        is_prime_ref
    in
    Gsunit.test
      ~name:"other tests"
      ~max_score:4.
      (`Multi
         [
           t 42;
           t 42222221;
           t 23;
         ])

  let tests =
    Gsunit.group
      ~name
      ~max_score
      [
        test;
        other_tests;
      ]
end

let () =
  [
    Sqrt.tests;
    Is_prime.tests;
  ]
  |> Gsunit.suite
  |> Gsunit.run
