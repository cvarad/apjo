open Apjo.Lib

let split (s : string) : char * string = s.[0], String.sub s 1 (String.length s - 1)

let letter c =
  Parser
    (fun s ->
      if s = ""
      then None
      else (
        match split s with
        | c', s' when c' = c -> Some (c', s')
        | _ -> None))
;;

match parse (letter 'c') "chocolate" with
| Some (_, s) -> print_endline s
| None -> print_endline "None";;