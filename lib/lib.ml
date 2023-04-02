type js_value =
  | JSString of string
  | JSNumber of int (* Float support later! *)
  | JSObject of (string * js_value) list
  | JSArray of js_value list (* list prohibits random-access! *)
  | JSBool of bool
  | JSNull

type 'a parser = Parser of (string -> ('a * string) option)

(* val js_value_parser : js_value parser *)
let js_value_parser : js_value parser = Parser(fun _ -> None)

let parse (Parser f) s = f s

module type Functor = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type Applicative = sig
  include Functor

  val pure : 'a -> 'a t
  val fseq : ('a -> 'b) t -> 'a t -> 'b t
end

module ParserApplicative : Applicative = struct
  type 'a t = 'a parser

  let fmap f p =
    Parser
      (fun s ->
        match parse p s with
        | Some (a, s') -> Some (f a, s')
        | None -> None)
  ;;

  let pure a = Parser (fun s -> Some (a, s))

  let fseq t a =
    Parser
      (fun s ->
        match parse t s with
        | Some (g, s') -> parse (fmap g a) s'
        | None -> None)
  ;;
end