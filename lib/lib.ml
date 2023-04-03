type js_value =
  | JSString of string
  | JSNumber of int (* Float support later! *)
  | JSObject of (string * js_value) list
  | JSArray of js_value list (* list prohibits random-access! *)
  | JSBool of bool
  | JSNull

module type Functor = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

module type Applicative = sig
  include Functor

  val pure : 'a -> 'a t
  val fseq : ('a -> 'b) t -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
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
  type 'a parser = Parser of (string -> ('a * string) option)
  type 'a t = 'a parser

  let mk f = Parser f
  let apply (Parser f) s = f s

  let fmap f (Parser p) =
    mk
    @@ fun s ->
    match p s with
    | Some (a, s') -> Some (f a, s')
    | None -> None
  ;;

  let ( <$> ) = fmap
  let pure a = Parser (fun s -> Some (a, s))

  let fseq (Parser t) a =
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

  let ( <|> ) (Parser a) (Parser b) =
    mk
    @@ fun s ->
    match a s with
    | Some (v, s') -> Some (v, s')
    | None -> b s
  ;;
end

let split (s : string) : char * string = s.[0], String.sub s 1 (String.length s - 1)

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

let js_null_p = (fun _ -> JSNull) <$> string_parser "null"

let js_bool_p =
  (fun b -> if b = "true" then JSBool true else JSBool false)
  <$> (string_parser "true" <|> string_parser "false")
;;

let js_value_p = js_null_p <|> js_bool_p