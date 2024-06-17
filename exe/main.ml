open Lib

let example = "forall _ : nat -> nat, forall y, forall z, P"

let parse_string str : (Syntax.t, Syntax.syntaxError) result =
  let lexbuf = Lexing.from_string ~with_positions:true str in
  try Ok (Parser.main Lexer.token lexbuf)
  with Syntax.SyntaxError err -> Error err

let () =
  match parse_string example with
  | Ok term -> Format.printf "Success :\n%a\n" Syntax.pp term
  | Error err -> Format.printf "Syntax error :\n%a\n" Syntax.pp_syntaxError err
