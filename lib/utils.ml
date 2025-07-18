type output_string_format =
  | Text
  | Html
  | Simple_format
  | Md
  | Ansi

type visibility =
  | Hidden
  | After_due_date
  | After_published
  | Visible
