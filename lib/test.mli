include Test_intf.Intf
open Utils

module Meta : sig
  type t
  val mk : (Formatted_string.t -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t

type 'a t
type test = SubTest.test list t
type result = SubTest.result list t

val mk : (Formatted_string.t -> 'a -> 'a t) with_options
val of_test_fun : (Formatted_string.t -> OUnitTest.test_fun -> test) with_options

val meta : 'a t -> Meta.t
val value : 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t

val to_ounit_test : test -> OUnitTest.test
val to_gradescope : string -> float -> result -> Gradescope.Test.t

val test_to_result : ounit_results -> test -> result
