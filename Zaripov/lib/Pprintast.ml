[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Pretty printer goes here *)

open Ast
open Utils

let pp =
  let open Format in
  let rec pp fmt = function
    | Var s -> Format.fprintf fmt "%s" s
    | Const n -> Format.fprintf fmt "%d" n
    | App (l, r, xs) ->
      Format.fprintf fmt "%a" pp l;
      r :: xs |> List.iter (fun expr -> Format.fprintf fmt " %a" pp expr)
    | Abs (x, t) -> Format.fprintf fmt "(fun %s -> %a)" x pp t
    | Let (var, assign_part, in_part) ->
      Format.fprintf fmt "(let %s = %a in %a)" var pp assign_part pp in_part
    | If (cond, then_expr, else_expr) ->
      Format.fprintf fmt "(if %a then %a else %a)" pp cond pp then_expr pp else_expr
  in
  pp
;;

let pp = pp
