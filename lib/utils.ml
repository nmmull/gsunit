type ounit_results = (int list * [ `Passed | `Failed ]) list

type ounit_test_runner = OUnitTest.(test -> result_list)

let default_ounit_test_runner ?(debug=false) () =
  let conf = OUnitConf.default () in
  let logger =
    if debug
    then OUnitLoggerStd.create conf OUnitLogger.shard_default
    else OUnitLogger.null_logger in
  let runner = OUnitRunner.of_name "processes" in
  let chooser = OUnitChooser.of_name "simple" in
  OUnitCore.run_test_tt conf logger runner chooser

let reformat_ounit_results results =
  let reformat_path =
    let rec go acc = function
      | [] -> acc
      | OUnitTest.ListItem i :: path -> go (i :: acc) path
      | _ :: path -> go acc path
    in go []
  in
  let reformat_result = function
    | OUnitTest.RSuccess -> `Passed
    | _ -> `Failed
  in
  List.map
    (fun (path, result, _) ->
      reformat_path path, reformat_result result)
    results

let results_by_index i =
  List.fold_left
    (fun acc (path, result) ->
      match path with
      | j :: path when i = j -> (path, result) :: acc
      | _ -> acc)
    []

type output_string_format =
  [ `Text
  | `Html
  | `Simple_format
  | `Md
  | `Ansi
  ]

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

let opt_of_visibility = function
  | `Visible -> None
  | v -> Some v

let opt_of_format = function
  | `Text -> None
  | f -> Some f

let json_of_format = function
  | `Text -> `String "text"
  | `Html -> `String "html"
  | `Simple_format -> `String "simple_format"
  | `Md -> `String "md"
  | `Ansi -> `String "ansi"

let json_of_status = function
  | `Passed -> `String "passed"
  | `Failed -> `String "failed"

let json_of_visibility = function
  | `Hidden -> `String "hidden"
  | `After_due_date -> `String "after_due_date"
  | `After_published -> `String "after_published"
  | `Visible -> `String "visible"

let json_of_float f = `Float f
let json_of_int n = `Int n
let json_of_string s = `String s
let json_of_tags t = `List (List.map json_of_string t)

type formatted_string = string * output_string_format

let str (s, _) = s
let format (_, f) = f
let format_str s f = (s, f)

let text s = (s, `Text)
let html s = (s, `Html)
let simple_format s = (s, `Simple_format)
let md s = (s, `Md)
let ansi s = (s, `Ansi)

module type WITH_META = sig
  type meta
  type 'a t

  val mk : meta -> 'a -> 'a t
  val meta : 'a t -> meta
  val value : 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module With_meta (M : sig type t end) = struct
  type 'a t = M.t * 'a

  let mk m a = (m, a)
  let meta (m, _) = m
  let value (_, a) = a
  let map f (m, a) = (m, f a)
end

(* https://discuss.ocaml.org/t/rounding-floats-to-number-of-decimals/6921 *)
let ceil3 x =
  if x -. (Float.round x) = 0.
  then x
  else
    floor (x *. 1000. +. 1.0) /. 1000.

let floor3 x =
  if x -. (Float.round x) = 0.
  then x
  else
    floor (x *. 1000.) /. 1000.

type group_name_formatter = formatted_string -> formatted_string

let default_group_name_formatter group_name_str name =
  match format name, group_name_str with
  | `Text, Some group_name_str -> text ("[" ^ group_name_str ^ "] " ^ str name)
  | _ -> name

let hidden_name = "<hidden>"
