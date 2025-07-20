type t = string * Gradescope.output_string_format

let str (s, _) = s
let format (_, f) = f

let text s = (s, `Text)
let html s = (s, `Html)
let simple_format s = (s, `Simple_format)
let md s = (s, `Md)
let ansi s = (s, `Ansi)
