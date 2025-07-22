include SubTest_intf.Intf
open Utils

module Meta : sig
  type t
  val mk : (string -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t
include WITH_META with type meta := Meta.t

type case = OUnitTest.test_fun
type test = case t
type result = [ `Passed | `Failed ] t

val mk: (string -> case -> test) with_options
val to_ounit_test : test -> OUnitTest.test
