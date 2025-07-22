open Utils

type 'a with_options =
  ?max_score:float ->
  ?hint:string ->
  ?hidden:bool ->
  ?number:string ->
  ?output:formatted_string ->
  ?status:status ->
  ?visibility:visibility ->
  ?tags:string list ->
  ?extra_data: Yojson.Basic.t ->
  'a

module type META = sig
  type t
  val max_score : t -> float option
  val name : t -> formatted_string
  val name_str : t -> string
  val name_format : t -> output_string_format
  val number : t -> string option
  val output : t -> formatted_string option
  val output_str : t -> string option
  val output_format : t -> output_string_format option
  val status : t -> status option
  val tags : t -> string list option
  val visibility : t -> visibility
  val extra_data : t -> Yojson.Basic.t option
  val hint : t -> string option
  val hidden : t -> bool
end

module type Intf = sig
  type nonrec 'a with_options = 'a with_options
  module type META = META
end
