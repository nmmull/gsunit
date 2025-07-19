open Utils

exception MissingTestMaxScore
exception InvalidGroupMaxScore

module Meta :sig
  type t

  val mk : string -> float -> t
  val name: t -> string
  val max_score : t -> float
end

type t

val mk : ?max_score:float -> string -> Test.t list -> t

val meta : t -> Meta.t
val test : t -> Test.t list

val name : t -> string
val max_score : t -> float

val to_ounit_test : t -> OUnitTest.test
val to_gradescope : ounit_results -> t -> Gradescope.Test.t list
