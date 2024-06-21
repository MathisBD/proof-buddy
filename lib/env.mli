(** This module defines the global environment, 
    which holds definitions and assumptions. *)

open Utils.Pervasive
open Ast

type entry = { name : Name.t; type_ : Term.ty; def : Term.t option }

(** The abstract type of environments. *)
type t

(** The empty environment. *)
val empty : t

(** Add a constant to the environment. *)
val add_constant : entry -> t -> t

val mem : Name.t -> t -> bool
val lookup : Name.t -> t -> entry option
val lookup_type : Name.t -> t -> Term.ty option
val lookup_def : Name.t -> t -> Term.t option
