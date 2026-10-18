[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type lowered =
  | Var of string
  | Abs of string * lowered
  | App of lowered * lowered * lowered list
  | Let of string * lowered * lowered
  | Const of int
[@@deriving show]

module StringMap = Map.Make (String)

type env = string StringMap.t

module State = struct
  type 'a t = env -> 'a * env

  let return (x : 'a) : 'a t = fun env -> x, env
  let pure = return

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun env ->
    let v, env' = m env in
    f v env'
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind
  let ( >> ) x y = bind x (fun _ -> y)
  let get : env t = fun env -> env, env

  let local (change : env -> env) (m : 'a t) : 'a t =
    fun env ->
    let ret, _ = Fun.compose m change env in
    ret, env
  ;;

  let run ((x, _) : 'a * env) = x

  let rec mapM (f : 'a -> 'b t) (lst : 'a list) =
    match lst with
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = mapM f xs in
      return (y :: ys)
  ;;
end

let ( <|> ) x def =
  match x with
  | Some x -> x
  | None -> def
;;

let addedGarbage = "#"
let recVar var = addedGarbage ^ var
let ifCheckName = addedGarbage ^ "if"

let z_combinator =
  let addedGarbage = "^" in
  let rec mistify_vars = function
    | Ast.Abs (x, t) -> Ast.Abs (addedGarbage ^ x, mistify_vars t)
    | Ast.App (l, r, lst) ->
      Ast.App (mistify_vars l, mistify_vars r, List.map mistify_vars lst)
    | Ast.Var x -> Ast.Var (addedGarbage ^ x)
    | _ -> Ast.Var "#Unreachable"
  in
  Parser.parse "fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> (x x v)))"
  |> Result.get_ok
  |> mistify_vars
;;

let lower ast =
  let open State in
  let rec helper = function
    | Ast.Const x -> return @@ Const x
    | Ast.Var x ->
      let* env = get in
      let x' = StringMap.find_opt x env <|> x in
      return @@ Var x'
    | Ast.Abs (x, t) ->
      let* t = helper t in
      return @@ Abs (x, t)
    | Ast.App (l, r, xs) ->
      let* l = helper l in
      let* r = helper r in
      let* lst = mapM helper xs in
      return @@ App (l, r, lst)
    | Ast.Let (var, assign_part, in_part) ->
      let* assign_part = helper assign_part in
      let* in_part = helper in_part in
      return @@ Let (var, assign_part, in_part)
    | Ast.Letrec (var, assign_part, in_part) ->
      let* in_part = helper in_part in
      let new_var = recVar var in
      let* assign_part = local (StringMap.add var new_var) (helper assign_part) in
      let* fix = helper z_combinator in
      return @@ Let (var, App (fix, Abs (new_var, assign_part), []), in_part)
    | Ast.If (cond, then_part, else_part) ->
      let* cond = helper cond in
      let* then_part = helper then_part in
      let* else_part = helper else_part in
      return @@ App (Var ifCheckName, cond, [ then_part; else_part ])
  in
  run @@ helper ast StringMap.empty
;;
