module SubTest = SubTest
module Test = Test
module Group = Group
module Suite = Suite
module Gradescope = Gradescope

type status =
  [ `Passed
  | `Failed
  ]

type visibility =
  [ `Hidden
  | `After_due_date
  | `After_published
  | `Visible
  ]

type output_string_format =
  [ `Text
  | `Html
  | `Simple_format
  | `Md
  | `Ansi
  ]

type formatted_string = string * output_string_format

type result_formatter = SubTest.result list -> formatted_string option

type group_name_formatter = string option -> formatted_string -> formatted_string

type ounit_test_runner = ?debug:bool -> unit -> OUnitTest.test -> OUnitTest.result_list

val subtest :
  ?name:string ->
  ?hint:string ->
  ?length:float ->
  ?hidden:bool ->
  OUnitTest.test_fun -> SubTest.test

val test :
  ?max_score:float ->
  ?hint:string ->
  ?hidden:bool ->
  ?number:string ->
  ?output:string ->
  ?output_format:output_string_format ->
  ?status:status ->
  ?visibility:visibility ->
  ?tags:string list ->
  ?extra_data:Yojson.Basic.t ->
  ?result_formatter:result_formatter ->
  ?name_format:output_string_format ->
  ?name:string ->
  [ `Single of OUnitTest.test_fun | `Multi of SubTest.test list ] -> Test.test

val group :
  ?max_score:float ->
  ?name:string ->
  Test.test list -> Group.test

val suite :
  ?output:string ->
  ?output_format:output_string_format ->
  ?visibility:visibility ->
  ?stdout_visibility:visibility ->
  ?extra_data:Yojson.Basic.t ->
  Group.test list -> Suite.test

val check:
  ?name:string ->
  ?hint:string ->
  ?hidden:bool ->
  pp_in:'a Fmt.t ->
  pp_out:'b Fmt.t ->
  ('a -> 'b) ->
  string ->
  'a -> 'b -> Test.test

val check_ref:
  ?name:string ->
  ?hint:string ->
  ?hidden:bool ->
  pp_in:'a Fmt.t ->
  pp_out:'b Fmt.t ->
  ('a -> 'b) ->
  string ->
  ('a -> 'b) ->
  'a -> Test.test

val check_sub:
  ?name:string ->
  ?hint:string ->
  ?hidden:bool ->
  pp_in:'a Fmt.t ->
  pp_out: 'b Fmt.t ->
  ('a -> 'b) ->
  string ->
  'a -> 'b -> SubTest.test

val check_sub_ref:
  ?name:string ->
  ?hint:string ->
  ?hidden:bool ->
  pp_in:'a Fmt.t ->
  pp_out:'b Fmt.t ->
  ('a -> 'b) ->
  string ->
  ('a -> 'b) ->
  'a -> SubTest.test

val run :
  ?group_name_formatter:(string option -> formatted_string -> formatted_string) ->
  ?output_formatter:Test.output_formatter ->
  ?status_formatter:Test.status_formatter ->
  ?ounit_test_runner:ounit_test_runner ->
  Suite.test -> unit
