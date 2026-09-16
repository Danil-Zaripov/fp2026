[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type 'name t = 'name Ast.t =
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [λx.t] *)
  | App of 'name t * 'name t * 'name t list
  | Let of string * 'name t * 'name t
  | Const of int
  | If of 'name t * 'name t * 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string
