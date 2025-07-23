module type META = sig
  type t
  val name : t -> string option
  val hint : t -> string option
  val length : t -> float
  val hidden : t -> bool
end

type 'a with_options =
  ?name:string ->
  ?hint:string ->
  ?length:float ->
  ?hidden:bool ->
  'a

module type Intf = sig
  type nonrec 'a with_options = 'a with_options
  module type META = META
end
