(** This module defines the core language of proof-buddy. *)

open Utils.Pervasive

module Term : sig
  type level = Prop | Type [@@deriving show]

  (** A binder represents the name of a bound variable (for instance in a Lambda or in a Prod). 
      It is this name that is printed to the user (or and underscore in case of an anonymous binder). *)
  type binder = Anonymous | Named of Name.t [@@deriving show]

  type esubst

  (** The grammar of terms. This is essentially the Calculus of Constructions. 
    The type is private : use the smart constructors provided below. *)
  type t = private
    (* [Var n] is a local variable bound by a lambda abstraction or a dependent product.
       The integer [n] is a de Bruijn index, starting at 0.

       A variable which escapes the last binder is called a "loose" variabl. *)
    | Var of int
    (* [Cst c] is a global constant. Its type can be found in the environment *)
    | Cst of Name.t
    (* We have two sorts : Prop and Type, with the following typing judgements :
       |- Prop : Type
       |- Type : Type *)
    | Sort of level
    (* [App _ f [x1; ...; xn]] represents the application [f x1 ... xn].
       We maintain the invariant that [f] itself is not an application,
       and that the argument list is not empty. *)
    | App of t * t list
    (* [Lambda _ x ty body] represents the lambda abstraction [fun x : ty => body].
       The term [ty] is the type of [x], and [x] can appear as a BVar in [body]. *)
    | Lambda of binder * ty * t
    (* [Prod _ x A B] is the product [forall x : A, B].
       [A] is the type of [x], and [x] can appear as a BVar in [body]. *)
    | Prod of binder * ty * t
    (* [Subst t subst] is the explicit substitution t{subst}.
       We maintain the invariant that [t] is not itself of the form [Subst _ _]. *)
    | Subst of t * esubst
  [@@deriving show]

  (** [ty] is a synonym of [t] used to enhance readability. *)
  and ty = t

  (** Smart constructor for [Term.Var]. *)
  val mkVar : int -> t

  (** Smart constructor for [Term.Cst]. *)
  val mkCst : Name.t -> t

  (** Smart constructor for [Term.Sort]. *)
  val mkSort : level -> t

  (** Equivalent to [mkSort Prop]. *)
  val mkProp : t

  (** Equivalent to [mkSort Type]. *)
  val mkType : t

  (** Smart constructor for [Term.App], ensuring that the function is not an application. *)
  val mkApp : t -> t -> t

  (** Same as [mkApp] but with multiple arguments. 
    If the argument list is empty, simply returns the function. *)
  val mkApps : t -> t list -> t

  (** Smart constructor for [Term.Prod] in case the product is non-dependent.
      This computes the cached data. *)
  val mkArrow : t -> t -> t

  (** [mkArrows [t1; ...; tn]] constructs the arrow [t1 -> ... -> tn]. 
    It assumes that the list is nonempty (i.e. n >= 1). *)
  val mkArrows : t list -> t

  (** Smart constructor for [Term.Lambda]. This assumes that the body contains a loose [Var 0],
    and does not perform any sort of shifting. *)
  val mkLambda : binder -> ty -> t -> t

  (** Smart constructor for [Term.Prod]. This assumes that the body contains a loose [Var 0],
    and does not perform any sort of shifting. *)
  val mkProd : binder -> ty -> t -> t

  (** Smart constructor for [Term.Subst]. *)
  val mkSubst : t -> esubst -> t
end

module ESubst : sig
  type t = Term.esubst

  (** The identity substitution. *)
  val id : t

  (** [lift ofs] maps each de Bruijn index [n] to [n + ofs]. *)
  val lift : int -> t

  (** [lift1] is shorthand for [lift 1]. *)
  val lift1 : t

  (** [cons t subst] maps the de Bruijn index [0] to [t], and each
      de Bruijn index [n + 1] to [subst(n)]. *)
  val cons : Term.t -> t -> t

  (** Left-to-right composition. *)
  val compose : t -> t -> t

  (** Infix synonym for [cons]. *)
  val ( <: ) : Term.t -> t -> t

  (** Infix notation for left-to-right composition. *)
  val ( >>> ) : t -> t -> t

  (** Infix notation for right-to-left composition. *)
  val ( <<< ) : t -> t -> t
end

(** [push_subst t] pushes explicit subsitutions in [t]. 
    It does just enough work so that the head constructor is not [Subst]. *)
val push_subst : Term.t -> Term.t

(** [alpha_equiv t1 t2] checks whether [t1] and [t2] are alpha-equivalent,
    i.e. are equal up to binder names. 
    This assumes [t1] and [t2] live in the same context and pushes substitutions. *)
val alpha_equiv : Term.t -> Term.t -> bool
