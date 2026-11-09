[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type name = string

type 'name pattern =
  | PConstr of 'name * 'name pattern list
  | PValueName of 'name
  | PAny

and 'name match_branch = 'name pattern * 'name t

(** The main type for our AST (дерева абстрактного синтаксиса) *)
and 'name t =
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [λx.t] *)
  | App of 'name t * 'name t * 'name t list (** Application *)
  | Let of 'name * 'name t * 'name t (** Let expression *)
  | Letrec of 'name * 'name t * 'name t (** Let rec expression *)
  | Const of int (** Number literal *)
  | If of 'name t * 'name t * 'name t (** If expression *)
  | Typedecl of 'name * ('name * int) list * 'name t
  (** Type declaration: type .. = | .. of arity in ..*)
  | Match of 'name t * 'name match_branch list
  | Constr of 'name * 'name t list

(** In type definition above the 3rd constructor is intentionally without documentation
    to test linter *)
type expr = name t
(* Application [f g ] *)
