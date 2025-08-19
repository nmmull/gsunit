exception MissingTestMaxScore
exception InvalidGroupMaxScore

module type META = sig
  type t
  val name: t -> string option
  val max_score : t -> float option
end

type 'a with_options =
  ?max_score:float ->
  ?name: string ->
  'a

module type Intf = sig
  module type META = META
end
