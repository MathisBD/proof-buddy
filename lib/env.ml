open Utils.Pervasive
open Ast

type entry = { name : Name.t; type_ : Term.ty; def : Term.t option }
type t = { map : entry Name.Map.t }

let empty : t = { map = Name.Map.empty }
let add_constant entry env : t = { map = Name.Map.add entry.name entry env.map }
let mem name env : bool = Name.Map.mem name env.map
let lookup name env : entry option = Name.Map.find_opt name env.map

let lookup_type name env : Term.ty option =
  let open Utils.Monad.Option in
  let* entry = Name.Map.find_opt name env.map in
  return entry.type_

let lookup_def name env : Term.t option =
  let open Utils.Monad.Option in
  let* entry = Name.Map.find_opt name env.map in
  let* def = entry.def in
  return def
