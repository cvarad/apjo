type js_value =
  | JSString of string (* TODO: parse escaped strings *)
  | JSNumber of int (* Float support later! *)
  | JSObject of (string * js_value) list
  | JSArray of js_value list (* list prohibits random-access! *)
  | JSBool of bool
  | JSNull

module type Functor = sig
  type 'a t = string -> ('a * string) option

  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

module type Applicative = sig
  include Functor

  val pure : 'a -> 'a t
  val fseq : ('a -> 'b) t -> 'a t -> 'b t
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

  val mk : (string -> ('a * string) option) -> 'a t
  val apply : 'a t -> string -> ('a * string) option
end

module ParserApplicative : Parser = struct
  type 'a t = string -> ('a * string) option

  let mk f = f
  let apply f s = f s

  let fmap f p =
    mk
    @@ fun s ->
    match p s with
    | Some (a, s') -> Some (f a, s')
    | None -> None
  ;;

  let ( <$> ) = fmap
  let pure a s = Some (a, s)

  let fseq t a =
    mk
    @@ fun s ->
    match t s with
    | Some (g, s') -> apply (fmap g a) s'
    | None -> None
  ;;

  (* Alternate implementation of fseq that does not use fmap
     but does essentially the same thing that fmap does. This is 
     for ease of understanding fseq.
     TODO: Use the option monad to avoid pattern matching *)
  (* let fseq (Parser t) (Parser a) =
    mk
    @@ fun s ->
    match t s with
    | Some (f, s') ->
      (match a s' with
      | Some (v, s'') -> Some (f v, s'')
      | _ -> None)
    | _ -> None
  ;; *)
  let ( <*> ) = fseq

  let ( *> ) a b =
    mk
    @@ fun s ->
    match a s with
    | Some (_, s') -> b s'
    | None -> None
  ;;

  let ( <* ) a b =
    mk
    @@ fun s ->
    match a s with
    | Some (v, s') ->
      (match b s' with
      | Some (_, s'') -> Some (v, s'')
      | None -> None)
    | None -> None
  ;;

  let ( <|> ) a b =
    mk
    @@ fun s ->
    match a s with
    | Some (v, s') -> Some (v, s')
    | None -> b s
  ;;
end

let split s = s.[0], String.sub s 1 (String.length s - 1)

(* let is_digit c = match c with | '0' .. '9' -> true | _ -> false *)
let is_digit c =
  let v = Char.code c in
  v >= 48 && v < 58
;;

(* Takes a predicate f and a string s, and returns a pair of strings (s1, s2).
   s1 is the substring obtained after applying f on each char of s until the
   first false is returned by f.
   s2 is the remaining substring *)
let until (f : char -> bool) (s : string) : string * string =
  let n = String.length s in
  let rec aux i = if i < n && f s.[i] then aux (i + 1) else i in
  let i = aux 0 in
  String.sub s 0 i, String.sub s i (n - i)
;;

open ParserApplicative

(* TODO: avoid string split *)
let char_parser c =
  mk
  @@ fun s ->
  match s with
  | "" -> None
  | s ->
    let c', s' = split s in
    if c' = c then Some (c', s') else None
;;

(* A string parser that avoids string splits and concats
   Instead, it converts the input pattern to char list and uses that for 
   matching. TODO: tail recursive *)
let string_parser pattern =
  mk
  @@ fun s ->
  let pat_chars = List.init (String.length pattern) (String.get pattern) in
  let pat_parsers = List.map char_parser pat_chars in
  let chars_to_str l = String.of_seq (List.to_seq l) in
  let rec aux = function
    | [] -> pure []
    | p :: ps -> (fun c l -> c :: l) <$> p <*> aux ps
  in
  match apply (aux pat_parsers) s with
  | Some (parsed_chars, s') -> Some (chars_to_str parsed_chars, s')
  | _ -> None
;;

let until_parser f = mk @@ fun s -> Some (until f s)

let non_empty_str p =
  mk
  @@ fun s ->
  match p s with
  | Some ("", _) -> None
  | x -> x
;;

let ws_parser = until_parser (fun c -> List.mem c [ ' '; '\n'; '\r'; '\t' ])
let js_null_p = (fun _ -> JSNull) <$> string_parser "null"

let js_bool_p =
  (fun b -> if b = "true" then JSBool true else JSBool false)
  <$> (string_parser "true" <|> string_parser "false")
;;

let js_number_p =
  (fun s -> JSNumber (int_of_string s)) <$> non_empty_str (until_parser is_digit)
;;

let js_string_p : string -> (js_value * string) option =
  (fun s -> JSString s)
  <$> char_parser '"' *> until_parser (( <> ) '"')
  <* char_parser '"'
;;

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

let array_elements p delim = (fun v l -> v :: l) <$> p <*> many (delim *> p) <|> pure []

(* Need to explicitly mention the argument 's' for mutually recursive
   definitions. Otherwise, the compiler won't know it's a function. *)
let rec js_array_p s =
  let delim = ws_parser *> char_parser ',' <* ws_parser in
  let array_parser =
    (fun v -> JSArray v)
    <$> (char_parser '[' *> ws_parser *> array_elements js_value_p delim
        <* ws_parser
        <* char_parser ']')
  in
  array_parser s

and js_value_p s =
  (js_null_p <|> js_bool_p <|> js_number_p <|> js_string_p <|> js_array_p) s
;;