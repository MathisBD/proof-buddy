{ 
open Utils.Pervasive
open Syntax
open Parser

(** Convert a [Lexing.position] to a [Position.t]. *)
let convert_lexpos (lexpos : Lexing.position) : Position.t =
  { pos_fname = lexpos.pos_fname
  ; pos_char = lexpos.pos_cnum
  ; pos_line = lexpos.pos_lnum
  ; pos_column = lexpos.pos_cnum - lexpos.pos_bol
  }


let keywords = 
  Hashtbl.of_list
    [ ( "fun", Fun )
    ; ( "forall", Forall )
    ; ( "Type", Type )
    ; ( "Prop", Prop )
    ] 
}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let whitespace = [' ' '\t']*

let newline = "\n" | "\r" | "\r\n"

rule token = 
  parse
  | whitespace { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "--" { comment lexbuf }
  | "(" { LParen }
  | ")" { RParen }
  | ":" { Colon }
  | "@" { At }
  | "," { Comma }
  | "_" { Underscore }
  | "->" { ThinArrow }
  | "=>" { FatArrow }
  | ident as id 
    { match Hashtbl.find_option keywords id with 
      | None -> Ident id
      | Some keyword -> keyword }
  | eof { Eof }
  | _ 
    { raise @@ SyntaxError (UnexpectedToken (
        Lexing.lexeme lexbuf, 
        convert_lexpos @@ Lexing.lexeme_start_p lexbuf, 
        convert_lexpos @@ Lexing.lexeme_end_p lexbuf)) }

and comment = parse 
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { raise @@ SyntaxError (UnexpectedEOF (
      convert_lexpos @@ Lexing.lexeme_start_p lexbuf)) }
  | _ { comment lexbuf }

