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

val output : Meta.t -> formatted_string option
val output_str : Meta.t -> string option
val output_format : Meta.t -> Gradescope.output_string_format option
val visibility : Meta.t -> Gradescope.visibility
val stdout_visibility : Meta.t -> Gradescope.visibility
val extra_data : Meta.t -> Yojson.Basic.t option

type 'a t
type test = Group.test list t
type result = Group.result list t

val mk : ('a -> 'a t) with_options

val meta : 'a t -> Meta.t
val value : 'a t -> 'a

val to_ounit_test : test -> OUnitTest.test
val to_gradescope : result -> Gradescope.Suite.t
