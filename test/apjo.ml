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