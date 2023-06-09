type js_value =
  | JSString of string
  | JSNumber of float
  | JSObject of (string * js_value) list
  | JSArray of js_value list
  | JSBool of bool
  | JSNull

val load_string : string -> js_value
val load_file : string -> js_value
val dump_string : js_value -> string
val dump_file : js_value -> string -> unit
val obj_get : string -> js_value -> js_value
val arr_nth : int -> js_value -> js_value