open Utils.Pervasive

module Term = struct
  type level = Prop | Type [@@deriving show]
  type binder = Anonymous | Named of Name.t [@@deriving show]

  type t =
    | Var of int
    | Cst of Name.t
    | Sort of level
    | App of t * t list
    | Lambda of binder * ty * t
    | Prod of binder * ty * t
    | Subst of t * esubst
  [@@deriving show]

  and esubst =
    | ELift of int
    | ECons of t * esubst
    | (* This is left to right composition. *)
      EComp of esubst * esubst

  and ty = t

  (* We define a module ESubst inside Term but don't export it in ast.mli.
     Outside of the Term module below, we bring ESubst to the global namespace.
     This is a bit weird but in the end allows for a nicer api in ast.mli. *)
  module ESubst = struct
    type t = esubst

    let id : t = ELift 0
    let lift n : t = ELift n
    let lift1 : t = ELift 1
    let cons t subst : t = ECons (t, subst)

    let rec compose (s1 : t) (s2 : t) : t =
      match (s1, s2) with
      | ELift 0, s | s, ELift 0 -> s
      | ELift n1, ELift n2 -> ELift (n1 + n2)
      | ELift n1, ECons (_, s2) ->
          (* Here we know [n1 > 0]. *)
          compose (ELift (n1 - 1)) s2
      | s1, s2 -> EComp (s1, s2)

    let ( <: ) t subst = cons t subst
    let ( >>> ) s1 s2 = compose s1 s2
    let ( <<< ) s2 s1 = compose s1 s2
  end

  let mkVar n =
    assert (n >= 0);
    Var n

  let mkCst name = Cst name
  let mkSort level = Sort level
  let mkProp = Sort Prop
  let mkType = Sort Type

  let mkApp f arg =
    match f with
    | App (f, f_args) -> App (f, f_args @ [ arg ])
    | _ -> App (f, [ arg ])

  let mkApps f args =
    if args = []
    then f
    else
      match f with
      | App (f, f_args) -> App (f, f_args @ args)
      | _ -> App (f, args)

  let mkSubst t subst =
    match t with
    | Subst (t, subst') -> Subst (t, ESubst.(subst' >>> subst))
    | _ -> Subst (t, subst)

  let mkArrow t1 t2 = Prod (Anonymous, t1, mkSubst t2 ESubst.lift1)

  let mkArrows ts =
    match List.rev ts with
    | [] -> failwith "Term.mkArrows : got an empty list."
    | t :: ts -> List.fold_right mkArrow (List.rev ts) t

  let mkLambda binder ty body = Lambda (binder, ty, body)
  let mkProd binder ty body = Prod (binder, ty, body)
end

module ESubst = Term.ESubst

let rec push_subst (term : Term.t) : Term.t =
  match term with
  | Subst (body, subst) -> push_esubsts_aux subst body
  | _ -> term

and push_esubsts_aux (subst : ESubst.t) (term : Term.t) : Term.t =
  match (term, subst) with
  (* Variables. *)
  | Var n, ELift ofs -> Term.mkVar (n + ofs)
  | Var n, ECons (x, subst) ->
      if n = 0
      then push_subst x
      else push_esubsts_aux subst @@ Term.mkVar (n - 1)
  | Var n, EComp (s1, s2) ->
      Term.mkVar n |> push_esubsts_aux s1 |> push_esubsts_aux s2
  (* Trivial cases. *)
  | Cst c, _ -> Term.mkCst c
  | Sort level, _ -> Term.mkSort level
  (* Recursive cases : we don't compute deeper. *)
  | App (f, args), subst ->
      Term.mkApps (Term.mkSubst f subst)
        (List.map (Fun.flip Term.mkSubst subst) args)
  | Lambda (x, ty, body), subst ->
      let body_subst = ESubst.(Term.mkVar 0 <: (subst >>> lift1)) in
      Term.mkLambda x (Term.mkSubst ty subst) (Term.mkSubst body body_subst)
  | Prod (x, ty, body), subst ->
      let body_subst = ESubst.(Term.mkVar 0 <: (subst >>> lift1)) in
      Term.mkProd x (Term.mkSubst ty subst) (Term.mkSubst body body_subst)
  (* Substitutions. *)
  | Subst (term, subst2), subst1 ->
      push_esubsts_aux ESubst.(subst2 >>> subst1) term

let rec alpha_equiv t1 t2 =
  match (push_subst t1, push_subst t2) with
  | Var n1, Var n2 when n1 = n2 -> true
  | Cst c1, Cst c2 when Name.equal c1 c2 -> true
  | Sort l1, Sort l2 when l1 = l2 -> true
  | App (f1, args1), App (f2, args2) when List.length args1 = List.length args2
    ->
      List.for_all2 alpha_equiv (f1 :: args1) (f2 :: args2)
  | Lambda (_, ty1, body1), Lambda (_, ty2, body2)
  | Prod (_, ty1, body1), Prod (_, ty2, body2) ->
      alpha_equiv ty1 ty2 && alpha_equiv body1 body2
  | _ -> false
