open Utils

module Test_intf = struct
    type 'a with_options =
      ?score:float ->
      ?max_score:float ->
      ?status:status ->
      ?name:string ->
      ?name_format:output_string_format ->
      ?number:string ->
      ?output:string ->
      ?output_format:output_string_format ->
      ?tags:string list ->
      ?visibility:visibility ->
      ?extra_data:Yojson.Basic.t ->
      'a

    module type TEST = sig
      type t
      val score : t -> float option
      val max_score: t -> float option
      val status : t -> status option
      val name : t -> string option
      val name_format : t -> output_string_format option
      val number : t -> string option
      val output : t -> string option
      val output_format : t -> output_string_format option
      val tags : t -> string list option
      val visibility : t -> visibility option
      val extra_data : t -> Yojson.Basic.t option
    end

    module type Intf = sig
      type nonrec 'a with_options = 'a with_options
      module type TEST = TEST
    end
end

module Suite_intf (Test : Test_intf.TEST) = struct

  exception MissingTopLevelScore
  exception MissingTestScore

  type 'a with_options =
    ?score:float ->
    ?execution_time:int ->
    ?output:string ->
    ?output_format:output_string_format ->
    ?test_output_format:output_string_format ->
    ?test_name_format:output_string_format ->
    ?visibility:visibility ->
    ?stdout_visibility:visibility ->
    ?tests:Test.t list ->
    ?extra_data:Yojson.Basic.t ->
    'a

  module type SUITE = sig
    type t
    val score : t -> float option
    val execution_time : t -> int option
    val output : t -> string option
    val output_format : t -> output_string_format option
    val test_output_format : t -> output_string_format option
    val test_name_format : t -> output_string_format option
    val visibility : t -> visibility option
    val stdout_visibility : t -> visibility option
    val tests : t -> Test.t list option
    val extra_data : t -> Yojson.Basic.t option
  end

  module type Intf = sig
    type nonrec 'a with_options = 'a with_options
    module type SUITE = SUITE
  end
end
