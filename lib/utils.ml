type output_string_format =
  | Text
  | Html
  | Simple_format
  | Md
  | Ansi

type output_formatter =
  (SubTest.Meta.t * OUnitTest.result) list ->
  (string * output_string_format) option

let default_output_formatter _ = None

type formatted_string =
  string * output_string_format

let text s = (s, Text)
let html s = (s, Html)
let simple_format s = (s, Simple_format)
let md s = (s, Md)
let ansi s = (s, Ansi)

type visibility =
  | Hidden
  | After_due_date
  | After_published
  | Visible
