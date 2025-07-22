open Utils

exception MissingTestMaxScore
exception InvalidGroupMaxScore

module type META = sig
  type t
  val name: t -> string
  val max_score : t -> float
end

module Meta : sig
  type t
  val mk : string -> float -> t
  include META with type t := t
end

include META with type t := Meta.t

type 'a t
type test = Test.test list t
type result = Test.result list t

val mk : string -> float -> 'a -> 'a t
val of_tests : ?max_score:float -> string -> Test.test list -> test

val meta : 'a t -> Meta.t
val value : 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t

val to_ounit_test : test -> OUnitTest.test
val to_gradescope : result -> Gradescope.Test.t list

val test_to_result : ounit_results -> test -> result
