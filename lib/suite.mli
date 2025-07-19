open Utils

type 'a with_options =
  ?output:formatted_string ->
  ?visibility:Gradescope.visibility ->
  ?stdout_visibility:Gradescope.visibility ->
  ?extra_data:Yojson.Basic.t ->
  'a

module Meta : sig
  type t

  val mk : (unit -> t) with_options

  val output : t -> formatted_string option
  val output_str : t -> string option
  val output_format : t -> Gradescope.output_string_format option
  val visibility : t -> Gradescope.visibility
  val stdout_visibility : t -> Gradescope.visibility
  val extra_data : t -> Yojson.Basic.t option
end

type t

val mk : (Group.t list -> t) with_options

val meta : t -> Meta.t
val groups : t -> Group.t list

val output : t -> formatted_string option
val output_str : t -> string option
val output_format : t -> Gradescope.output_string_format option
val visibility : t -> Gradescope.visibility
val stdout_visibility : t -> Gradescope.visibility
val extra_data : t -> Yojson.Basic.t option

val to_ounit_test : t -> OUnitTest.test
