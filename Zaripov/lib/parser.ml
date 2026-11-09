[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Angstrom

let is_space = Char.Ascii.is_white
let ( @>@ ) c x = String.of_char c ^ x

let digits =
  let digits = take_while1 Char.Ascii.is_digit in
  digits <|> lift2 ( @>@ ) (char '-') digits
;;

let spaces = skip_while is_space
let spaces_mandatory = skip_many1 (satisfy is_space)

let varname =
  lift2 ( @>@ ) (satisfy Char.Ascii.is_lower) (take_while Char.Ascii.is_letter)
;;

let consname =
  lift2 ( @>@ ) (satisfy Char.Ascii.is_upper) (take_while Char.Ascii.is_letter)
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

module StringSet = Set.Make (String)

let keywords =
  [ "fun"; "in"; "let"; "if"; "then"; "else"; "rec"; "match"; "with"; "type"; "of" ]
  |> StringSet.of_list
;;

let parse_var =
  varname
  >>= fun x ->
  if not @@ StringSet.mem x keywords
  then return @@ Ast.Var x
  else fail "Keyword used as ident"
;;

let parse_const = digits >>= fun x -> return @@ Ast.Const (int_of_string x)

let parse_let_nonrec expr =
  string "let" *> spaces *> varname
  >>= fun var ->
  spaces *> string "=" *> spaces *> expr
  >>= fun assign_part ->
  spaces *> string "in" *> spaces *> expr
  >>= fun in_part -> return @@ Ast.Let (var, assign_part, in_part)
;;

let parse_letrec expr =
  let* var = string "let" *> spaces *> string "rec" *> spaces *> varname in
  let* assign_part = spaces *> string "=" *> spaces *> expr in
  let* in_part = spaces *> string "in" *> spaces *> expr in
  return @@ Ast.Letrec (var, assign_part, in_part)
;;

let parse_let expr = parse_let_nonrec expr <|> parse_letrec expr

let parse_cons_decl_single =
  let* cons_name = consname in
  let* arity =
    spaces_mandatory *> string "of" *> spaces_mandatory *> take_while1 Char.Ascii.is_digit
    >>| int_of_string
  in
  return [ cons_name, arity ]
;;

let parse_cons_decl =
  chainl1
    parse_cons_decl_single
    (spaces *> string "|" *> spaces *> return (fun l r -> l @ r))
;;

let parse_type expr =
  let* typename =
    string "type" *> spaces_mandatory *> varname <* spaces <* string "=" <* spaces
  in
  let* cons_lst = parse_cons_decl in
  let* following_expr = spaces *> string "in" *> spaces_mandatory *> expr in
  return @@ Ast.Typedecl (typename, cons_lst, following_expr)
;;

let parse_pattern =
  fix (fun pattern ->
    let constr =
      let* name = consname in
      let* lst =
        spaces
        *> string "("
        *> spaces
        *> chainl1
             (pattern >>| List.singleton)
             (spaces *> string "," *> spaces *> return (fun l r -> l @ r))
        <* spaces
        <* string ")"
      in
      return @@ Ast.PConstr (name, lst)
    in
    let constr_empty =
      let* name = consname in
      let* _ = spaces *> string "(" *> spaces *> string ")" *> spaces <|> spaces in
      return @@ Ast.PConstr (name, [])
    in
    let valname =
      let* name = varname in
      return @@ Ast.PValueName name
    in
    let wildcard = string "_" *> return Ast.PAny in
    constr <|> constr_empty <|> valname <|> wildcard)
;;

let parse_match_branch_single expr =
  let* pattern = parse_pattern in
  let* following_expr = spaces *> string "->" *> spaces *> expr in
  return (pattern, following_expr)
;;

let parse_match_branches expr =
  string "|"
  *> spaces
  *> chainl1
       (parse_match_branch_single expr >>| List.singleton)
       (spaces *> string "|" *> spaces *> return (fun l r -> l @ r))
;;

let parse_match expr =
  let* match_expr =
    spaces *> string "match" *> spaces_mandatory *> expr
    <* spaces_mandatory
    <* string "with"
    <* spaces_mandatory
  in
  let* match_branches = parse_match_branches expr in
  return @@ Ast.Match (match_expr, match_branches)
;;

let parse_constr expr =
  let* name = consname in
  let empty_constr =
    spaces *> string "(" *> spaces *> string ")" *> spaces
    <|> spaces
    >>= fun () -> return @@ Ast.Constr (name, [])
  in
  (let* expr_lst =
     spaces
     *> string "("
     *> chainl1
          (expr >>| List.singleton)
          (spaces *> string "," *> spaces *> return (fun l r -> l @ r))
     <* spaces
     <* string ")"
   in
   return @@ Ast.Constr (name, expr_lst))
  <|> empty_constr
;;

let parse_fun expr =
  string "fun" *> spaces *> varname
  >>= fun name ->
  spaces *> string "->" *> spaces *> expr >>= fun body -> return @@ Ast.Abs (name, body)
;;

let parse_if expr =
  string "if" *> spaces *> expr
  >>= fun cond ->
  spaces *> string "then" *> spaces *> expr
  >>= fun then_expr ->
  spaces *> string "else" *> spaces *> expr
  >>= fun else_expr -> return @@ Ast.If (cond, then_expr, else_expr)
;;

let parse_app atom =
  atom
  >>= fun main ->
  spaces *> atom
  >>= fun first ->
  many (spaces *> atom) >>= fun lst -> return @@ Ast.App (main, first, lst)
;;

let parse_paren_expr expr = string "(" *> spaces *> expr <* spaces <* string ")"

let parse_atom expr =
  conde
    [ parse_var
    ; parse_const
    ; parse_let expr
    ; parse_fun expr
    ; parse_if expr
    ; parse_paren_expr expr
    ; parse_constr expr
    ; parse_type expr
    ; parse_match expr
    ]
;;

let operator op =
  spaces *> string op *> spaces *> (return @@ fun l r -> Ast.App (Ast.Var op, l, [ r ]))
;;

let add = operator "+"
let sub = operator "-"
let mul = operator "*"
let div = operator "/"
let le = operator "<"
let ge = operator ">"
let leq = operator "<="
let geq = operator ">="
let eq = operator "="
let neq = operator "<>"
let or_op = operator "||"
let and_op = operator "&&"

let binop expr =
  let atom = parse_atom expr in
  let factor = parse_app atom <|> atom in
  let term = chainl1 factor (mul <|> div) in
  let ineq = chainl1 term (add <|> sub) in
  let equality = chainl1 ineq (leq <|> geq <|> le <|> ge) in
  let ands = chainl1 equality (eq <|> neq) in
  let ors = chainl1 ands and_op in
  chainl1 ors or_op
;;

let parse_expr = fix binop

type error = [ `Parsing_error of string ]

let parse str =
  match Angstrom.parse_string parse_expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
