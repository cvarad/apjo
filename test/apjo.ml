open Apjo.Lib
open ParserApplicative

(* Test null *)
let () = assert((apply js_value_p "null") = Some(JSNull, ""));;
(* Test bool true *)
let () = assert((apply js_value_p "true") = Some((JSBool true), ""));;
(* Test bool false *)
let () = assert((apply js_value_p "false") = Some((JSBool false), ""));;
(* Test number *)
let () = assert((apply js_value_p "52345") = Some((JSNumber 52345), ""));;
(* Test string *)
let () = assert((apply js_string_p "\"hello\"") = Some((JSString "hello"), ""));;
(* Test array *)
let () = assert((apply js_value_p "[52345]") = Some((JSArray [JSNumber 52345]), ""));;
(* Test empty array *)
let () = assert((apply js_value_p "[]") = Some((JSArray []), ""));;
(* Test object *)
let () = assert((apply js_value_p "{\"hello\": 5345}") = Some((JSObject [("hello", JSNumber 5345)]), ""));;
(* Test empty object *)
let () = assert((apply js_value_p "{}") = Some((JSObject []), ""));;
(* Multi-value test *)
let json_string =
"{\
  \"key1\": [1, 2, true, {}],\
  \"key2\": { \"key3\": null}\
}" in
let json_struct =
  JSObject
    [ "key1", JSArray [ JSNumber 1; JSNumber 2; JSBool true; JSObject [] ]
    ; "key2", JSObject [ "key3", JSNull ]
    ]
in
assert((apply js_value_p json_string) = Some(json_struct, ""))