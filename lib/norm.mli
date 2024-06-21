open Ast

(** Reduce a term using the given reduction strategy. *)
val reduce : [ `WHNF | `NF ] -> Term.t -> Term.t

(** Check if two terms are convertible modulo beta-eta reduction. 
    This assumes both terms have the same type (they can contain loose variables 
    but must live in the same context). *)
val convertible : Env.t -> Term.t -> Term.t -> bool
