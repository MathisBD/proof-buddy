open Ast

(** Reduce a term using the given reduction strategy. *)
val reduce : [ `Weak | `Strong ] -> Term.t -> Term.t

(** Check if two terms are convertible modulo beta-eta reduction. *)
val convertible : Term.t -> Term.t -> bool
