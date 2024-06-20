open Lib

let example = "\nforall x : nat,\n  P x y ->\n    forall a (b : B) c d, P a"

let parse_string str : (Syntax.term, Syntax.syntaxError) result =
  let lexbuf = Lexing.from_string ~with_positions:true str in
  try Ok (Parser.main Lexer.token lexbuf)
  with Syntax.SyntaxError err -> Error err

let () =
  match parse_string example with
  | Ok term -> Format.printf "Success :\n%a\n" Syntax.pp_term term
  | Error err -> Format.printf "Syntax error :\n%a\n" Syntax.pp_syntaxError err
