exception MissingTestMaxScore
exception InvalidGroupMaxScore

module type META = sig
  type t
  val name: t -> string
  val max_score : t -> float
end

module type Intf = sig
  module type META = META
end
