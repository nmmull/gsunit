open Utils

type 'a with_options =
  ?output:Formatted_string.t ->
  ?visibility:visibility ->
  ?stdout_visibility:visibility ->
  ?extra_data:Yojson.Basic.t ->
  'a

module type META = sig
  type t
  val output : t -> Formatted_string.t option
  val output_str : t -> string option
  val output_format : t -> output_string_format option
  val visibility : t -> visibility
  val stdout_visibility : t -> visibility
  val extra_data : t -> Yojson.Basic.t option
end

module type Intf = sig
  type nonrec 'a with_options = 'a with_options
  module type META = META
end
