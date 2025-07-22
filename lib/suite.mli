include Suite_intf.Intf
open Utils

module Meta : sig
  type t
  val mk : (unit -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t

type 'a t
type test = Group.test list t
type result = Group.result list t

val mk : ('a -> 'a t) with_options

val meta : 'a t -> Meta.t
val value : 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t

val to_ounit_test : test -> OUnitTest.test
val to_gradescope : result -> Gradescope.Suite.t

val test_to_result : ounit_results -> test -> result
