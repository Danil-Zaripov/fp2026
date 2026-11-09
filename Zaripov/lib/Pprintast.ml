[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Pretty printer goes here *)

open Ast

let rec pp_pattern fmt = function
  | PAny -> Format.fprintf fmt "_"
  | PValueName name -> Format.fprintf fmt "%s" name
  | PConstr (name, pat_list) ->
    Format.fprintf fmt "%s(" name;
    let rec helper = function
      | x :: x' :: xs ->
        Format.fprintf fmt "%a, " pp_pattern x;
        helper @@ (x' :: xs)
      | x :: [] -> Format.fprintf fmt "%a" pp_pattern x
      | _ -> ()
    in
    helper pat_list;
    Format.fprintf fmt ")"
;;

let pp =
  let rec pp fmt = function
    | Var s -> Format.fprintf fmt "%s" s
    | Const n -> Format.fprintf fmt "%d" n
    | App (l, r, xs) ->
      Format.fprintf fmt "(%a" pp l;
      r :: xs |> List.iter (fun expr -> Format.fprintf fmt " %a" pp expr);
      Format.fprintf fmt ")"
    | Abs (x, t) -> Format.fprintf fmt "(fun %s -> %a)" x pp t
    | Let (var, assign_part, in_part) ->
      Format.fprintf fmt "(let %s = %a in %a)" var pp assign_part pp in_part
    | Letrec (var, assign_part, in_part) ->
      Format.fprintf fmt "(let rec %s = %a in %a)" var pp assign_part pp in_part
    | If (cond, then_expr, else_expr) ->
      Format.fprintf fmt "(if %a then %a else %a)" pp cond pp then_expr pp else_expr
    | Constr (name, expr_lst) ->
      Format.fprintf fmt "(%s(" name;
      let rec helper = function
        | x :: x' :: xs ->
          Format.fprintf fmt "%a, " pp x;
          helper @@ (x' :: xs)
        | x :: [] -> Format.fprintf fmt "%a" pp x
        | [] -> ()
      in
      helper expr_lst;
      Format.fprintf fmt "))"
    | Match (mexpr, branches) ->
      Format.fprintf fmt "(match %a with " pp mexpr;
      branches
      |> List.iter (fun (pat, bexpr) ->
        Format.fprintf fmt "| %a -> %a" pp_pattern pat pp bexpr);
      Format.fprintf fmt ")"
    | Typedecl (name, decl_lst, following_expr) ->
      Format.fprintf fmt "type %s = " name;
      let rec helper = function
        | (constr_name, arity) :: x' :: xs ->
          Format.fprintf fmt "%s of %d | " constr_name arity;
          helper @@ (x' :: xs)
        | (constr_name, arity) :: [] -> Format.fprintf fmt "%s of %d" constr_name arity
        | [] -> ()
      in
      helper decl_lst;
      Format.fprintf fmt " %a" pp following_expr
  in
  pp
;;

let pp = pp
