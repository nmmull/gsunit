exception MissingTestMaxScore
exception InvalidGroupMaxScore

module Meta = struct
  type t =
    {
      name: string;
      max_score: float;
    }

  let mk name max_score = {name; max_score}

  let name m = m.name
  let max_score m = m.max_score
end

type t = Meta.t * Test.t list

let max_score_tests tests =
  let rec go (num, acc) = function
    | [] -> (num, acc)
    | test :: tests ->
       match Test.max_score test with
       | None -> go (num, acc) tests
       | Some max_score -> go (num + 1, max_score +. acc) tests
  in go (0, 0.) tests

let mk ?max_score name tests =
  let max_score =
    match max_score with
    | Some max_score ->
       if snd (max_score_tests tests) > max_score
       then raise InvalidGroupMaxScore
       else max_score
    | None ->
       let rec go acc = function
         | [] -> Some acc
         | test :: tests ->
            match Test.max_score test with
            | None -> None
            | Some max_score -> go (acc +. max_score) tests
       in
       match go 0. tests with
       | None -> raise MissingTestMaxScore
       | Some max_score -> max_score
  in
  (Meta.mk name max_score, tests)

let meta (m, _) = m
let name (m, _) = Meta.name m
let max_score (m, _) = Meta.max_score m
let tests (_, t) = t
let num_tests t = List.length (tests t)
let num_max_score_given (_, t) = fst (max_score_tests t)
let max_score_given (_, t) = snd (max_score_tests t)
let num_remainder t = num_tests t - num_max_score_given t
let max_score_remainder t =
  (max_score t -. max_score_given t)
  /. float_of_int (num_remainder t)

let to_ounit_test group =
  group
  |> tests
  |> List.map Test.to_ounit_test
  |> OUnit2.(>:::) (name group)

let to_gradescope test_results group =
  let default_max_score = max_score_remainder group in
  List.mapi
    (fun i test ->
      let test_results =
        test_results
        |> Utils.results_by_index i
        |> List.map
             (fun (nodes, result) ->
               match nodes with
               | [OUnitTest.Label _; OUnitTest.ListItem j] ->
                  SubTest.meta (List.nth (Test.sub_tests test) j), result
               | _ -> assert false)
      in
      let group_name =
        if num_tests group = 1
        then None
        else Some (name group)
     in
      Test.to_gradescope
        ?group_name
        default_max_score
        test_results
        test)
    (tests group)
