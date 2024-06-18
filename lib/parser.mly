%{
open Utils.Pervasive
open Syntax

(** Convert a [Lexing.position] to a [Position.t]. *)
let convert_lexpos (lexpos : Lexing.position) : Position.t =
  { pos_fname = lexpos.pos_fname
  ; pos_char = lexpos.pos_cnum
  ; pos_line = lexpos.pos_lnum
  ; pos_column = lexpos.pos_cnum - lexpos.pos_bol
  }
%}

(*******************************************************************************)
(** Tokens. *)
(*******************************************************************************)

%token LParen RParen Colon At Comma Underscore ThinArrow FatArrow 
%token Fun Forall Type Prop
%token <string> Ident
%token Eof

%start <t> main
%%

(*******************************************************************************)
(** Rules. *)
(*******************************************************************************)

main: t=term_bind Eof { t }

term_bind:
| t=term_arrow { t }
| Fun bs=binder_list FatArrow body=withpos(term_bind) 
  { List.fold_right (fun (x, ty) body -> Lambda (x, ty, body)) bs body }
| Forall bs=binder_list Comma body=withpos(term_bind) 
  { List.fold_right (fun (x, ty) body -> Prod (x, ty, body)) bs body }

term_arrow:
| t=term_app { t }
| t1=withpos(term_app) ThinArrow t2=withpos(term_bind) { Prod (Anonymous, Some t1, t2) }

term_app:
| t=atom { t }
| f=withpos(atom) args=withpos(atom)+ { App (f, args) }

atom:
| ident=Ident { Id (Name.make ident) }
| Prop { Sort Prop }
| Type { Sort Type }
| LParen t=withpos(term_bind) At ty=withpos(term_bind) RParen { Ann (t, ty) }
| LParen t=withpos(term_bind) RParen { t }
| error 
  { raise @@ SyntaxError (ParsingError (
      convert_lexpos $startpos, 
      convert_lexpos $endpos)) }

(* Handling binders is a bit tedious. 
   We want to allow all of the following syntax :
     forall x, P
     forall x : T, P
     forall x (y : T) x t (u : U), P
     forall (x : T), P
*)

(* Parenthese around [x : T] can be omitted if it is the only binder in the list. *)
binder_list:
| x=binder_name Colon ty=withpos(term_bind) { [(x, Some ty)] }
| bs=binder+ { bs }

binder:
| x=binder_name { (x, None) }
| LParen x=binder_name Colon ty=withpos(term_bind) RParen { (x, Some ty) }

(* An underscore corresponds to an anonymous binder. *)
binder_name:
| Underscore { Anonymous }
| ident=Ident { Named (Name.make ident) }

(* Add position information to a rule that produces terms. *)
withpos(term_rule):
| t=term_rule { Pos (t, convert_lexpos $startpos, convert_lexpos $endpos) }
