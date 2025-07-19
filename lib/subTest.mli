type 'a with_options =
  ?hint:string ->
  ?length:float ->
  ?hidden:bool ->
  'a

module type META = sig
  type t
  val name : t -> string
  val hint : t -> string option
  val length : t -> float
  val hidden : t -> bool
end

module Meta : sig
  type t
  val mk : (string -> t) with_options
  include META with type t := t
end

include META with type t := Meta.t

type 'a t
type test = OUnitTest.test_fun t
type result = OUnitTest.result t

val mk: (string -> 'a -> 'a t) with_options
val meta : 'a t -> Meta.t
val value : 'a t -> 'a
val to_ounit_test : test -> OUnitTest.test
