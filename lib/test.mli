include Test_intf.Intf
open Utils

module Meta : sig
  type t
  val mk : (unit -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t
include WITH_META with type meta := Meta.t

val name_default : Meta.t -> string

type case =
  [ `Single of OUnitTest.test_fun
  | `Multi of SubTest.test list
  ]

type test = SubTest.test list t
type result = SubTest.result list t

val mk : ('a -> 'a t) with_options

val update_meta : ('a t -> 'a t) with_options

val of_case : (case -> test) with_options

val to_ounit_test : test -> OUnitTest.test

val test_to_result :
  ounit_results -> test -> result

type output_formatter = result -> formatted_string option
type status_formatter = result -> status option

val default_output_formatter : output_formatter
val default_status_formatter : status_formatter

val to_gradescope :
  ?output_formatter:output_formatter ->
  ?status_formatter:status_formatter ->
  ?default_max_score:float ->
  group_name_formatter:(formatted_string -> formatted_string) ->
  result ->
  Gradescope.Test.t
