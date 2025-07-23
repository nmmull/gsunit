module type META = sig
  type t
  val name : t -> string
  val hint : t -> string option
  val length : t -> float
  val hidden : t -> bool
end

type 'a with_options =
  ?hint:string ->
  ?length:float ->
  ?hidden:bool ->
  name:string ->
  'a

module type Intf = sig
  type nonrec 'a with_options = 'a with_options
  module type META = META
end
