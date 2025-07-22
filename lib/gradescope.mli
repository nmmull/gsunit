module Test : sig
  include Gradescope_intf.Test_intf.Intf

  type t
  include TEST with type t := t
  val mk : (unit -> t) with_options
  val to_json : t -> Yojson.Basic.t
end

module Suite : sig
  include Gradescope_intf.Suite_intf (Test).Intf

  type t
  include SUITE with type t := t

  val mk : (unit -> t) with_options
  val to_json : t -> Yojson.Basic.t
end
