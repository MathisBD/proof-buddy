%{
open Utils.Pervasive
open Syntax
%}

(*******************************************************************************)
(** Tokens. *)
(*******************************************************************************)

%token LParen RParen Colon At Comma Underscore ThinArrow FatArrow 
%token Fun Forall Type Prop
%token <string> Ident
%token Eof

%nonassoc fun_prec forall_prec
%left ThinArrow
%nonassoc At

%start <t> main
%%

(*******************************************************************************)
(** Rules. *)
(*******************************************************************************)

main: t=term Eof { t }

term:
| ident=Ident { Id (Name.make ident) }
| Prop { Sort Prop }
| Type { Sort Type }
| Fun xty=typed_binder FatArrow body=term %prec fun_prec 
  { let x, ty = xty in Lambda (x, ty, body) }
| Forall xty=typed_binder Comma body=term %prec forall_prec 
  { let x, ty = xty in Prod (x, ty, body) }
| t1=term ThinArrow t2=term { Prod (Anonymous, Some t1, t2) } 
| t=term At ty=term { Ann (t, ty) }
| LParen t=term RParen { t }
(*| t1=term t2=term %prec app_prec { App (t1, [ t2 ]) }*)
(*| error { failwith "SyntaxError : TODO" (* ($startpos, $endpos) *) }*)

typed_binder:
| x=binder Colon ty=term { (x, Some ty) }
| x=binder { (x, None) }

binder:
| Underscore { Anonymous }
| ident=Ident { Named (Name.make ident) }
