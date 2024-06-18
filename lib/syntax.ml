(** This module defines the syntax of surface terms as input by the user. *)

open Utils.Pervasive

(***************************************************************************)
(** Source positions. *)

(** We don't use [Lexing.position] directly in order to decouple 
    the implementation a bit from ocamllex. *)
module Position = struct
  type t =
    { (* The name of the file. *)
      pos_fname : string
    ; (* The absolute character offset since the start of the file. *)
      pos_char : int
    ; (* The line number. *)
      pos_line : int
    ; (* The character offset from the start of the current line. *)
      pos_column : int
    }
  [@@deriving show]
end

(***************************************************************************)
(** Syntax errors. *)

type syntaxError =
  | UnexpectedToken of string * Position.t * Position.t
  | UnexpectedEOF of Position.t
  | ParsingError of Position.t * Position.t
[@@deriving show]

exception SyntaxError of syntaxError

(***************************************************************************)
(** Core syntax. *)

type level = Type | Prop [@@deriving show]

type binder =
  (* An anonymous binder. We can't refer to the variable this binds.
     This is used in the following situations :
     - When the user enters [_] for the binder name, as in [fun _ => 42].
     - For non-dependent products, as in [nat -> nat].
  *)
  | Anonymous
  (* A named binder. *)
  | Named of Name.t
[@@deriving show]

(** This type represents the syntax of surface terms. *)
type t =
  (* A raw identifier. This could be a local variable or a global constant.
     The task of resolving the identifier (i.e. scoping) is part of type checking. *)
  | Id of Name.t
  (* A sort. *)
  | Sort of level
  (* Lambda abstraction. The type annotation on the binder is optional. *)
  | Lambda of binder * ty option * t
  (* (Dependent) product. The type annotation on the binder is optional. *)
  | Prod of binder * ty option * t
  (* Function application. We maintain the invariant that the argument list
     is non-empty and that the function is not itself an application. *)
  | App of t * t list
  (* Type annotation. *)
  | Ann of t * ty
  (* Attach a start and end position to a term.
     This is used to provide better error messages to the user. *)
  | Pos of t * Position.t * Position.t
[@@deriving show]

(** To enhance readability we define a synonym for terms that are to be 
    interpreted as types. *)
and ty = t [@@deriving show]
