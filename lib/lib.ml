exception IllegalArgumentException

type js_value =
  | JSString of string (* TODO: parse escaped strings *)
  | JSNumber of float
  | JSObject of (string * js_value) list
  | JSArray of js_value list
  | JSBool of bool
  | JSNull

module type Functor = sig
  type s = char list
  type 'a t = s -> ('a * s) option

  (* fmap *)
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

module type Applicative = sig
  include Functor

  val pure : 'a -> 'a t

  (* fseq *)
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
end

module type Alternative = sig
  include Applicative

  val ( <|> ) : 'a t -> 'a t -> 'a t
end

module type Parser = sig
  include Alternative
end

module ParserApplicative : Parser = struct
  type s = char list
  type 'a t = s -> ('a * s) option

  let fmap f p s =
    match p s with
    | Some (a, s') -> Some (f a, s')
    | None -> None
  ;;

  let ( <$> ) = fmap
  let pure a s = Some (a, s)

  let fseq t a s =
    match t s with
    | Some (g, s') -> (fmap g a) s'
    | None -> None
  ;;

  (* Alternate implementation of fseq that does not use fmap
     but does essentially the same thing that fmap does. This is 
     for ease of understanding fseq.
     TODO: Use the option monad to avoid pattern matching *)
  (* let fseq (t) (a) =
    fun s ->
    match t s with
    | Some (f, s') ->
      (match a s' with
      | Some (v, s'') -> Some (f v, s'')
      | _ -> None)
    | _ -> None
  ;; *)
  let ( <*> ) = fseq

  let ( *> ) a b s =
    match a s with
    | Some (_, s') -> b s'
    | None -> None
  ;;

  let ( <* ) a b s =
    match a s with
    | Some (v, s') ->
      (match b s' with
      | Some (_, s'') -> Some (v, s'')
      | None -> None)
    | None -> None
  ;;

  let ( <|> ) a b s =
    match a s with
    | Some (v, s') -> Some (v, s')
    | None -> b s
  ;;
end

let is_digit c =
  let v = Char.code c in
  v >= 48 && v < 58
;;

open ParserApplicative

(* Takes a predicate f and a string s, and returns a pair of strings (s1, s2).
   s1 is the substring obtained after applying f on each char of s until the
   first false is returned by f.
   s2 is the remaining substring *)
let until (f : char -> bool) : string t =
 fun s ->
  let rec aux acc s =
    match s with
    | c :: s' when f c -> aux (c :: acc) s'
    | _ -> String.of_seq (List.to_seq (List.rev acc)), s
  in
  Some (aux [] s)
;;

(* TODO: avoid string split *)
let char_parser c s =
  match s with
  | x :: s' when c = x -> Some (c, s')
  | _ -> None
;;

(* A string parser that avoids string splits and concats
   Instead, it converts the input pattern to char list and uses that for 
   matching. TODO: tail recursive *)
let string_parser pattern s =
  let pat_chars = List.init (String.length pattern) (String.get pattern) in
  let pat_parsers = List.map char_parser pat_chars in
  let chars_to_str l = String.of_seq (List.to_seq l) in
  let rec aux = function
    | [] -> pure []
    | p :: ps -> (fun c l -> c :: l) <$> p <*> aux ps
  in
  match (aux pat_parsers) s with
  | Some (parsed_chars, s') -> Some (chars_to_str parsed_chars, s')
  | _ -> None
;;

let until_parser f : string t = fun s -> until f s

let non_empty_str p : string t =
 fun s ->
  match p s with
  | Some ("", _) -> None
  | x -> x
;;

let ws_parser = until_parser (fun c -> List.mem c [ ' '; '\n'; '\r'; '\t' ])

let string_literal_parser =
  char_parser '"' *> until_parser (( <> ) '"') <* char_parser '"'
;;

let js_null_p = (fun _ -> JSNull) <$> string_parser "null"

let js_bool_p =
  (fun b -> if b = "true" then JSBool true else JSBool false)
  <$> (string_parser "true" <|> string_parser "false")
;;

let js_number_p =
  (fun a b c -> JSNumber (float_of_string (a ^ b ^ c)))
  <$> (string_parser "-" <|> pure "")
  <*> non_empty_str (until_parser is_digit)
  <*> ((fun a b -> a ^ b)
      <$> string_parser "."
      <*> non_empty_str (until_parser is_digit)
      <|> pure "")
;;

let js_string_p = (fun s -> JSString s) <$> string_literal_parser

let many p s =
  let rec aux s =
    match p s with
    | Some (v, s') ->
      let l, s'' = aux s' in
      v :: l, s''
    | None -> [], s
  in
  Some (aux s)
;;

let elements p delim = (fun v l -> v :: l) <$> p <*> many (delim *> p) <|> pure []

(* Need to explicitly mention the argument 's' for mutually recursive
   definitions. Otherwise, the compiler won't know it's a function. *)
let rec js_array_p s =
  let delim = ws_parser *> char_parser ',' <* ws_parser in
  let array_parser =
    (fun v -> JSArray v)
    <$> (char_parser '[' *> ws_parser *> elements js_value_p delim
        <* ws_parser
        <* char_parser ']')
  in
  array_parser s

and js_object_p s =
  let delim = ws_parser *> char_parser ',' <* ws_parser in
  let object_values =
    (fun s v -> s, v)
    <$> (string_literal_parser <* ws_parser <* char_parser ':' <* ws_parser)
    <*> js_value_p
  in
  ((fun v -> JSObject v)
  <$> ws_parser *> char_parser '{' *> ws_parser *> elements object_values delim
  <* ws_parser
  <* char_parser '}'
  <* ws_parser)
    s

and js_value_p s =
  (js_null_p <|> js_bool_p <|> js_number_p <|> js_string_p <|> js_array_p <|> js_object_p)
    s
;;

let load_string s =
  match js_value_p (List.of_seq (String.to_seq s)) with
  | Some (v, []) -> v
  | _ -> failwith "Parsing error!"
;;

(* TODO: Add error handling *)
let load_file f =
  let ic = open_in f in
  let s = really_input_string ic (in_channel_length ic) in
  let _ = close_in ic in
  load_string s
;;

let rec string_of_list l = "[" ^ String.concat ", " (List.map dump_string l) ^ "]"

and string_of_object o =
  let l = List.map (fun (k, v) -> "\"" ^ k ^ "\": " ^ dump_string v) o in
  "{" ^ String.concat ", " l ^ "}"

and dump_string json =
  match json with
  | JSNull -> "null"
  | JSBool b -> Bool.to_string b
  | JSNumber n -> Printf.sprintf "%g" n
  | JSString s -> "\"" ^ s ^ "\""
  | JSArray a -> string_of_list a
  | JSObject o -> string_of_object o
;;

let dump_file json f =
  let oc = open_out f in
  let () = Printf.fprintf oc "%s" (dump_string json) in
  close_out oc
;;

let obj_get k = function
  | JSObject o -> List.assoc k o
  | _ -> raise IllegalArgumentException
;;

let arr_nth n = function
  | JSArray a -> List.nth a n
  | _ -> raise IllegalArgumentException
;;
