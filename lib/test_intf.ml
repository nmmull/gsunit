open Utils

type 'a with_options =
  ?max_score:float ->
  ?hint:string ->
  ?hidden:bool ->
  ?number:string ->
  ?output:string ->
  ?output_format:output_string_format ->
  ?status:status ->
  ?visibility:visibility ->
  ?tags:string list ->
  ?extra_data: Yojson.Basic.t ->
  ?result_formatter: (SubTest.result list -> formatted_string option) ->
  ?name_format:output_string_format ->
  ?name:string ->
  'a

module type META = sig
  type t
  val max_score : t -> float option
  val name : t -> string option
  val name_format : t -> output_string_format
  val number : t -> string option
  val output : t -> string option
  val output_format : t -> output_string_format
  val status : t -> status option
  val tags : t -> string list option
  val visibility : t -> visibility
  val extra_data : t -> Yojson.Basic.t option
  val hint : t -> string option
  val hidden : t -> bool
  val result_formatter : t -> SubTest.result_formatter option
end

module type Intf = sig
  type nonrec 'a with_options = 'a with_options
  module type META = META
end
