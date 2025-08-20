module type META = sig
  type t
  val name : t -> string option
  val length : t -> float
end

type 'a with_options =
  ?name:string ->
  ?length:float ->
  'a

module type Intf = sig
  type nonrec 'a with_options = 'a with_options
  module type META = META
end
