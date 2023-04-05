type js_value =
  | JSString of string
  | JSNumber of int
  | JSObject of (string * js_value) list
  | JSArray of js_value list
  | JSBool of bool
  | JSNull

val load_string : string -> js_value
val load_file : string -> js_value
val dump_string : js_value -> string
val dump_file : js_value -> string -> unit