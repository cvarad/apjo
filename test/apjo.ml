open Apjo.Lib

(* Test null *)
let () = assert (load_string "null" = JSNull)

(* Test bool true *)
let () = assert (load_string "true" = JSBool true)

(* Test bool false *)
let () = assert (load_string "false" = JSBool false)

(* Test number *)
let () = assert (load_string "52345" = JSNumber 52345)

(* Test string *)
let () = assert (load_string "\"hello\"" = JSString "hello")

(* Test array *)
let () = assert (load_string "[52345]" = JSArray [ JSNumber 52345 ])

(* Test empty array *)
let () = assert (load_string "[]" = JSArray [])

(* Test object *)
let () = assert (load_string "{\"hello\": 5345}" = JSObject [ "hello", JSNumber 5345 ])

(* Test empty object *)
let () = assert (load_string "{}" = JSObject []);;

(* Multi-value test *)
let json_string = "{\"key1\": [1, 2, true, {}],\"key2\": { \"key3\": null}}" in
let json_struct =
  JSObject
    [ "key1", JSArray [ JSNumber 1; JSNumber 2; JSBool true; JSObject [] ]
    ; "key2", JSObject [ "key3", JSNull ]
    ]
in
assert (load_string json_string = json_struct)

(* Read from file test *)
let json_struct =
  JSObject
    [ ( "events"
      , JSArray
          [ JSObject
              [ "name", JSString "Artist A concert"
              ; "cost", JSNumber 450
              ; "time", JSString "2023-04-05T06:50:38.703Z"
              ]
          ; JSObject
              [ "name", JSString "Artist B concert"
              ; "cost", JSNumber 1000
              ; "time", JSString "2024-04-05T05:50:38.703Z"
              ]
          ] )
    ; "data", JSBool true
    ]
;;

let () = assert (load_file "test.json" = json_struct)

let () =
  assert (
    dump_file json_struct "write_read_test.json";
    load_file "write_read_test.json" = json_struct)
;;
