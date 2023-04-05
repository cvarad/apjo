type js_value =
  | JSString of string (* TODO: parse escaped strings *)
  | JSNumber of int (* Float support later! *)
  | JSObject of (string * js_value) list
  | JSArray of js_value list (* list prohibits random-access! *)
  | JSBool of bool
  | JSNull

val load_string : string -> js_value
val load_file : string -> js_value