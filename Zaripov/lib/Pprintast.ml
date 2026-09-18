[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Pretty printer goes here *)

open Ast
open Utils

let pp ?(compact = true) =
  let open Format in
  let mangle t fmt x =
    if is_free_in x t || not compact then fprintf fmt "%s" x else fprintf fmt "_"
  in
  let rec pp fmt = function
    | Var s -> Format.fprintf fmt "%s" s
    | App (l, r, xs) ->
      Format.fprintf fmt "%a" pp l;
      r :: xs |> List.iter (fun expr -> Format.fprintf fmt " %a" pp expr)
    | Abs (x, Abs (y, Var z)) when x = z && y <> z && compact ->
      if compact then Format.fprintf fmt "⊤"
    | Abs (x, Abs (y, Var z)) when y = z && x <> z && compact -> Format.fprintf fmt "⊥"
    | Abs (f, Abs (x, Var z)) when x = z && x <> f && compact -> Format.fprintf fmt "0"
    | Abs (f, Abs (x, App (Var g, Var z, []))) when x = z && x <> f && g = f && compact ->
      Format.fprintf fmt "1"
    | Abs (f, Abs (x, App (Var g, App (Var h, Var z, []), [])))
      when x = z && x <> f && g = f && h = g && compact -> Format.fprintf fmt "2"
    | Abs (v1, Abs (v2, Abs (v3, Abs (v4, t)))) when compact ->
      Format.fprintf
        fmt
        "(λ %a %a %a %a -> %a)"
        (mangle t)
        v1
        (mangle t)
        v2
        (mangle t)
        v3
        (mangle t)
        v4
        pp
        t
    | Abs (v1, Abs (v2, Abs (v3, t))) when compact ->
      Format.fprintf
        fmt
        "(λ %a %a %a -> %a)"
        (mangle t)
        v1
        (mangle t)
        v2
        (mangle t)
        v3
        pp
        t
    | Abs (v1, Abs (v2, t)) when compact ->
      Format.fprintf fmt "(λ %a %a -> %a)" (mangle t) v1 (mangle t) v2 pp t
    | Abs (x, t) -> Format.fprintf fmt "(λ %a . %a)" (mangle t) x pp t
    | Const n -> Format.fprintf fmt "%d" n
    | Let (var, assign_part, in_part) ->
      Format.fprintf fmt "let %s = %a in %a" var pp assign_part pp in_part
    | If (cond, then_expr, else_expr) ->
      Format.fprintf fmt "if %a then %a else %a" pp cond pp then_expr pp else_expr
  in
  pp
;;

let pp_hum = pp ~compact:true
let pp = pp ~compact:false
