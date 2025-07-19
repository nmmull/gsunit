open Utils

type 'a with_options =
  ?max_score:float ->
  ?hint:string ->
  ?hidden:bool ->
  ?number:string ->
  ?output_formatter:Utils.output_formatter ->
  ?status_checker:Utils.status_checker ->
  ?visibility:Gradescope.visibility ->
  ?tags:string list ->
  ?extra_data: Yojson.Basic.t ->
  'a

module Meta : sig
  type t
  val mk : (Utils.formatted_string -> t) with_options

  val max_score : t -> float option
  val name : t -> formatted_string
  val name_str : t -> string
  val name_format : t -> Gradescope.output_string_format
  val number : t -> string option
  val output_formatter : t -> output_formatter
  val status_checker : t -> status_checker
  val tags : t -> string list option
  val visibility : t -> Gradescope.visibility
  val extra_data : t -> Yojson.Basic.t option

end

type t


val of_sub_tests : (formatted_string -> SubTest.t list -> t) with_options
val of_test_fun : (formatted_string -> OUnitTest.test_fun -> t) with_options

val meta : t -> Meta.t
val sub_tests : t -> SubTest.t list

val max_score : t -> float option
val name : t -> formatted_string
val name_str : t -> string
val name_format : t -> Gradescope.output_string_format
val number : t -> string option
val output_formatter : t -> output_formatter
val status_checker : t -> status_checker
val tags : t -> string list option
val visibility : t -> Gradescope.visibility
val extra_data : t -> Yojson.Basic.t option

val to_ounit_test : t -> OUnitTest.test
val to_gradescope :
  ?group_name:string ->
  float ->
  test_results ->
  t ->
  Gradescope.Test.t
