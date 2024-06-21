open Utils.Pervasive
open Ast

(** This always produces a term that head-substitution-free. *)
let rec reduce strat term : Term.t =
  match push_subst term with
  (* Leaves. *)
  | Var n -> Term.mkVar n
  | Cst c -> Term.mkCst c
  | Sort l -> Term.mkSort l
  (* Binders. *)
  | Lambda (x, ty, body) ->
      if strat = `NF
      then Term.mkLambda x (reduce strat ty) (reduce strat body)
      else Term.mkLambda x ty body
  | Prod (x, ty, body) when strat = `NF ->
      if strat = `NF
      then Term.mkProd x (reduce strat ty) (reduce strat body)
      else Term.mkProd x ty body
  (* Applications. *)
  | App (f, args) -> begin
      match reduce strat f with
      (* Beta redex. *)
      | Lambda (_, _, body) ->
          (* We implement call by value : normalize the first argument before substituting. *)
          let arg = reduce strat @@ List.hd args in
          (* The de Bruijn index [0] is mapped to [arg],
             and any other de Bruijn indices [n] is mapped to [n-1]. *)
          let redex = Term.mkSubst body ESubst.(arg <: id) in
          (* Don't forget to remove the first argument from the list. *)
          reduce strat @@ Term.mkApps redex @@ List.tl args
      (* Neutral term. *)
      | f ->
          if strat = `WHNF
          then Term.mkApps f args
          else Term.mkApps f @@ List.map (reduce strat) args
    end
  | _ -> assert false

let rec convertible env t1 t2 : bool =
  (* We compare weak head normal forms. *)
  match (reduce `WHNF t1, reduce `WHNF t2) with
  (* Same WHNF. *)
  | Var n1, Var n2 when n1 = n2 -> true
  | Cst c1, Cst c2 when Name.equal c1 c2 -> true
  | Sort s1, Sort s2 when s1 = s2 -> true
  | Lambda (_, _, body1), Lambda (_, _, body2) ->
      (* No need to check the types of the binders are convertible,
         because the terms have the same type. *)
      convertible env body1 body2
  | Prod (_, ty1, body1), Prod (_, ty2, body2) ->
      convertible env ty1 ty2 && convertible env body1 body2
  | App (f1, args1), App (f2, args2) when List.length args1 = List.length args2
    ->
      List.for_all2 (convertible env) (f1 :: args1) (f2 :: args2)
  (* One side is a constant and the other is not : unfold the constant. *)
  | Cst c, t when Env.lookup_def c env != None ->
      let def = Option.get @@ Env.lookup_def c env in
      convertible env def t
  | t, Cst c when Env.lookup_def c env != None ->
      let def = Option.get @@ Env.lookup_def c env in
      convertible env t def
  (* One side is a lambda and the other is not : check for eta equivalence. *)
  | Lambda (_, _, body), t | t, Lambda (_, _, body) ->
      let t = Term.mkApp (Term.mkSubst t ESubst.lift1) (Term.mkVar 0) in
      convertible env body t
  (* The terms are not convertible. *)
  | _ -> false
