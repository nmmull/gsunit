include Group_intf.Intf
open Utils

type 'a with_options =
  ?max_score:float ->
  ?name: string ->
  'a

module Meta : sig
  type t
  val mk : (unit -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t
include WITH_META with type meta := Meta.t

type test = Test.test list t
type result = Test.result list t

val mk : ('a -> 'a t) with_options
val of_tests : (Test.test list -> test) with_options

val to_ounit_test : test -> OUnitTest.test
val to_gradescope :
  ?group_name_formatter:(string option -> group_name_formatter) ->
  ?output_formatter:Test.output_formatter ->
  ?status_formatter:Test.status_formatter ->
  result ->
  Gradescope.Test.t list

val test_to_result : ounit_results -> test -> result
