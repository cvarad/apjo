open Apjo.Lib

let test_null () = Alcotest.(check bool) "Parse null" true (load_string "null" = JSNull)

let test_true () =
  Alcotest.(check bool) "Parse true" true (load_string "true" = JSBool true)
;;

let test_false () =
  Alcotest.(check bool) "Parse false" true (load_string "false" = JSBool false)
;;

let test_number () =
  Alcotest.(check bool) "Parse number" true (load_string "52345" = JSNumber 52345.)
;;

let test_string () =
  Alcotest.(check bool) "Parse string" true (load_string "\"hello\"" = JSString "hello")
;;

let test_empty_array () =
  Alcotest.(check bool) "Parse array" true (load_string "[]" = JSArray [])
;;

let test_empty_object () =
  Alcotest.(check bool) "Parse object" true (load_string "{}" = JSObject [])
;;

let test_array () =
  Alcotest.(check bool)
    "Parse array"
    true
    (load_string "[52345]" = JSArray [ JSNumber 52345. ])
;;

let test_object () =
  Alcotest.(check bool)
    "Parse object"
    true
    (load_string "{\"hello\": 5345}" = JSObject [ "hello", JSNumber 5345. ])
;;

let test_multi_value () =
  let json_string = "{\"key1\": [1, 2, true, {}],\"key2\": { \"key3\": null}}" in
  let json_struct =
    JSObject
      [ "key1", JSArray [ JSNumber 1.; JSNumber 2.; JSBool true; JSObject [] ]
      ; "key2", JSObject [ "key3", JSNull ]
      ]
  in
  Alcotest.(check bool) "Parse object" true (load_string json_string = json_struct)
;;

let json_struct =
  JSObject
    [ ( "events"
      , JSArray
          [ JSObject
              [ "name", JSString "Artist A concert"
              ; "cost", JSNumber 450.
              ; "time", JSString "2023-04-05T06:50:38.703Z"
              ]
          ; JSObject
              [ "name", JSString "Artist B concert"
              ; "cost", JSNumber 1000.
              ; "time", JSString "2024-04-05T05:50:38.703Z"
              ]
          ] )
    ; "data", JSBool true
    ]
;;

let test_load_file () =
  Alcotest.(check bool) "Parse file" true (load_file "test.json" = json_struct)
;;

let test_dump_load () =
  Alcotest.(check bool)
    "Write & Read file"
    true
    (dump_file json_struct "write_read_test.json";
     load_file "write_read_test.json" = json_struct)
;;

let () =
  let open Alcotest in
  run
    "JSON Parser"
    [ ( "Basic Parsing"
      , [ test_case "Parse null" `Quick test_null
        ; test_case "Parse true" `Quick test_true
        ; test_case "Parse false" `Quick test_false
        ; test_case "Parse number" `Quick test_number
        ; test_case "Parse string" `Quick test_string
        ; test_case "Parse empty array" `Quick test_empty_array
        ; test_case "Parse empty object" `Quick test_empty_object
        ] )
    ; ( "Compound Parsing"
      , [ test_case "Parse array" `Quick test_array
        ; test_case "Parse oject" `Quick test_object
        ; test_case "Parse multi-value object" `Quick test_multi_value
        ; test_case "Parse file" `Quick test_load_file
        ; test_case "Read-Write test" `Quick test_dump_load
        ] )
    ]
;;
