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
| Fun x=binder Colon ty=term FatArrow body=term { Lambda (x, Some ty, body) }
| Fun x=binder FatArrow body=term { Lambda (x, None, body) }
| Forall x=binder Colon ty=term Comma body=term { Prod (x, Some ty, body) }
| Forall x=binder Comma body=term { Prod (x, None, body) }
| t1=term ThinArrow t2=term { Prod (Anonymous, Some t1, t2) } 
| t=term At ty=term { Ann (t, ty) }
| LParen t=term RParen { t }
(*| t1=term t2=term { App (t1, [ t2 ]) }*)
(*| error { failwith "SyntaxError : TODO" (* ($startpos, $endpos) *) }*)

binder:
| Underscore { Anonymous }
| ident=Ident { Named (Name.make ident) }
