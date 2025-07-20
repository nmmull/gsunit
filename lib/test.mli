open Utils

type 'a with_options =
  ?max_score:float ->
  ?hint:string ->
  ?hidden:bool ->
  ?number:string ->
  ?output:Formatted_string.t ->
  ?status:Gradescope.status ->
  ?visibility:Gradescope.visibility ->
  ?tags:string list ->
  ?extra_data: Yojson.Basic.t ->
  'a

module type META = sig
  type t
  val max_score : t -> float option
  val name : t -> Formatted_string.t
  val name_str : t -> string
  val name_format : t -> Gradescope.output_string_format
  val number : t -> string option
  val output : t -> Formatted_string.t option
  val output_str : t -> string option
  val output_format : t -> Gradescope.output_string_format option
  val status : t -> Gradescope.status option
  val tags : t -> string list option
  val visibility : t -> Gradescope.visibility
  val extra_data : t -> Yojson.Basic.t option
  val hint : t -> string option
  val hidden : t -> bool
end

module Meta : sig
  type t
  val mk : (Formatted_string.t -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t

type 'a t
type test = SubTest.test list t
type result = SubTest.result list t

val mk : (Formatted_string.t -> 'a -> 'a t) with_options
val of_test_fun : (Formatted_string.t -> OUnitTest.test_fun -> test) with_options

val meta : 'a t -> Meta.t
val value : 'a t -> 'a

val to_ounit_test : test -> OUnitTest.test
val to_gradescope : string -> float -> result -> Gradescope.Test.t

val test_to_result : ounit_results -> test -> result
