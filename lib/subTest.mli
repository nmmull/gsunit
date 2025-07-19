type 'a with_options =
  ?hint:string ->
  ?length:float ->
  ?hidden:bool ->
  'a

module Meta : sig
  type t

  val mk : (string -> t) with_options
  val name : t -> string
  val hint : t -> string option
  val length : t -> float
  val hidden : t -> bool
end

type t

val mk: (string -> OUnitTest.test_fun -> t) with_options
val meta : t -> Meta.t
val name : t -> string
val hint : t -> string option
val length : t -> float
val hidden : t -> bool
val test_fun : t -> OUnitTest.test_fun

val to_ounit_test : t -> OUnitTest.test
