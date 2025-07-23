include Group_intf.Intf
open Utils

module Meta : sig
  type t
  val mk : max_score:float -> name:string -> t
  include META with type t := t
end

include META with type t := Meta.t
include WITH_META with type meta := Meta.t

type test = Test.test list t
type result = Test.result list t

val mk : max_score:float -> name:string -> 'a -> 'a t
val of_tests : ?max_score:float -> name:string -> Test.test list -> test

val to_ounit_test : test -> OUnitTest.test
val to_gradescope :
  ?group_name_formatter:(string -> group_name_formatter) ->
  ?output_formatter:Test.output_formatter ->
  ?status_formatter:Test.status_formatter ->
  result ->
  Gradescope.Test.t list

val test_to_result : ounit_results -> test -> result
