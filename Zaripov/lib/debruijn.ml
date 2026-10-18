[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Builtin

type de_bruijn =
  | Const of int
  | Var of int
  | Abs of de_bruijn
  | App of de_bruijn * de_bruijn * de_bruijn list
  | Let of de_bruijn * de_bruijn
[@@deriving show]

module StringMap = Map.Make (String)

type env = int StringMap.t
type error = string

module Basis = struct
  type 'a t = env -> ('a * env, error) result

  let return (x : 'a) : 'a t = fun env -> Ok (x, env)

  let bind (m : 'a t) ~(f : 'a -> 'b t) : 'b t =
    fun env -> Result.bind (m env) (fun (x, env') -> (f x) env')
  ;;
end

module State = struct
  type 'a t = 'a Basis.t

  include Stdune.Monad.Make (Basis)

  let run = Result.map (fun (x, _) -> x)
  let get : env t = fun env -> Result.Ok (env, env)

  let local (change : env -> env) (m : 'a t) : 'a t =
    fun env -> Result.map (fun (ret, _) -> ret, env) (m (change env))
  ;;

  let fail (msg : string) = fun _ -> Error msg

  let rec mapM (f : 'a -> 'b t) (lst : 'a list) =
    let open O in
    match lst with
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = mapM f xs in
      return (y :: ys)
  ;;
end

let builtinMap = function
  | "#if" -> Some bIf
  | "=" -> Some bEq
  | "+" -> Some bAdd
  | "-" -> Some bSub
  | "&&" -> Some bAnd
  | "||" -> Some bOr
  | "*" -> Some bMul
  | "/" -> Some bDiv
  | "<" -> Some bLe
  | ">" -> Some bGe
  | "<=" -> Some bLeq
  | ">=" -> Some bGeq
  | "<>" -> Some bIneq
  | _ -> None
;;

let defaultWith f = function
  | Some x -> Some x
  | None -> f ()
;;

let to_debruijn ast =
  let open State in
  let open O in
  let promote x = Fun.compose (StringMap.add x 0) (StringMap.map (fun x -> x + 1)) in
  let rec helper = function
    | Normal.Const x -> return @@ Const x
    | Normal.Abs (x, t) ->
      let* t = local (promote x) (helper t) in
      return @@ Abs t
    | Normal.App (l, r, xs) ->
      let* l = local Fun.id (helper l) in
      let* r = local Fun.id (helper r) in
      let* xs = mapM (fun x -> local Fun.id (helper x)) xs in
      return @@ App (l, r, xs)
    | Normal.Var x ->
      let* map = get in
      (match defaultWith (fun () -> StringMap.find_opt x map) (builtinMap x) with
       | Some x -> return @@ Var x
       | _ -> fail "Undeclared name used")
    | Normal.Let (var, assign_part, in_part) ->
      let* assign_part = local Fun.id (helper assign_part) in
      let* in_part = local (promote var) (helper in_part) in
      return @@ Let (assign_part, in_part)
  in
  run @@ helper ast StringMap.empty
;;
