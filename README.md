![Build & Test](https://github.com/cvarad/apjo/actions/workflows/main.yml/badge.svg)

# APJO: Applicative-based Parser for JSON in OCaml
APJO is a JSON parsing library written in pure OCaml using immutable data structures. At its core, it is built using the concept of [Applicatives](https://en.wikipedia.org/wiki/Applicative_functor) and Functional Parsing.

Note: This parser was developed as a means to get used to OCaml, and to learn more about functional parsing.


## Usage
The library uses the following ADT to hold the JSON values:
```ocaml
type js_value =
  | JSString of string
  | JSNumber of float
  | JSObject of (string * js_value) list
  | JSArray of js_value list
  | JSBool of bool
  | JSNull
```

To parse a string with JSON in it, use the `load_string` function. Use the `load_file` function to read a file and parse its JSON content.
```ocaml
load_string "{\"hello\": 5345}";;
(* js_value = JSObject [("hello", JSNumber 5345.)] *)

load_file "hello.json";;
(* js_value = JSObject [("hello", JSNumber 5345.)] *)
```

Similarly, to create a string out of a JSON value, use the `dump_string` (or `dump_file`) function.
```ocaml
dump_string @@ JSObject [ "hello", JSNumber 5345. ];;
(* string = "{\"hello\": 5345}" *)

dump_file (JSObject [ "hello", JSNumber 5345. ]) "hello.json";;
(* unit = () *)
```