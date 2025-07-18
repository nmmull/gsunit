exception EmptyTestGroup

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

let mk name max_score tests =
  if List.is_empty tests
  then raise EmptyTestGroup
  else (Meta.mk name max_score, tests)

let name (m, _) = Meta.name m
let max_score (m, _) = Meta.max_score m
let tests (_, t) = t
