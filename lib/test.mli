include Test_intf.Intf
open Utils

module Meta : sig
  type t
  val mk : (unit -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t
include WITH_META with type meta := Meta.t

type case =
  [ `Single of OUnitTest.test_fun
  | `Multi of SubTest.test list
  ]
type test = SubTest.test list t
type result = SubTest.result list t

val mk : (case -> test) with_options

val to_ounit_test : test -> OUnitTest.test
val to_gradescope : string -> float -> result -> Gradescope.Test.t

val test_to_result : ounit_results -> test -> result
