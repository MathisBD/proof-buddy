open Ast

(** This always produces a term that head-substitution-free. *)
let rec reduce strength term : Term.t =
  match push_subst term with
  (* Leaves. *)
  | Var n -> Term.mkVar n
  | Cst c -> Term.mkCst c
  | Sort l -> Term.mkSort l
  (* Binders. *)
  | Lambda (x, ty, body) when strength = `Strong ->
      Term.mkLambda x ty @@ reduce strength body
  | Prod (x, ty, body) when strength = `Strong ->
      Term.mkProd x ty @@ reduce strength body
  (* Applications. *)
  | App (f, arg :: args) -> begin
      match reduce strength f with
      (* Beta redex. *)
      | Lambda (_, _, body) ->
          (* We implement call by value : normalize the argument first. *)
          let arg = reduce strength arg in
          (* The de Bruijn index [0] is mapped to [arg],
             and any other de Bruijn indices [n] is mapped to [n-1]. *)
          let redex = Term.mkSubst body ESubst.(arg <: id) in
          reduce strength @@ Term.mkApps redex args
      (* Neutral term. *)
      | f -> Term.mkApps f (arg :: args)
    end
  | term -> term

let convertible _t1 _t2 = failwith "Norm.convertible: todo"
