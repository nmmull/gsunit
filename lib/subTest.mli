include SubTest_intf.Intf
open Utils

module Meta : sig
  type t
  val mk : (unit -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t
include WITH_META with type meta := Meta.t

val name_default : Meta.t -> string

type test = OUnitTest.test_fun t
type result = [ `Passed | `Failed ] t

type result_formatter = result list -> formatted_string option

val mk : ('a -> 'a t) with_options
val of_test_fun : (OUnitTest.test_fun -> test) with_options
val to_ounit_test : test -> OUnitTest.test
