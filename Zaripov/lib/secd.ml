[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Builtin

type instr =
  | Access of int
  | Closure of instr list
  | Let
  | Endlet
  | Apply
  | Return
  | Const of int
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Ge
  | Le
  | Geq
  | Leq
  | Ineq
  | Or
  | And
  | If of instr list * instr list
[@@deriving show]

let isBuiltin n = n < 0

let getBuiltinInstr = function
  | n when n = bAdd -> Some Add
  | n when n = bSub -> Some Sub
  | n when n = bMul -> Some Mul
  | n when n = bDiv -> Some Div
  | n when n = bLe -> Some Le
  | n when n = bGe -> Some Ge
  | n when n = bLeq -> Some Leq
  | n when n = bGeq -> Some Geq
  | n when n = bOr -> Some Or
  | n when n = bAnd -> Some And
  | n when n = bIneq -> Some Ineq
  | n when n = bEq -> Some Eq
  | _ -> None
;;

let rec compile =
  let compile_lst f = List.concat_map (fun x -> f x @ [ Apply ]) in
  function
  | Debruijn.Var n -> [ Access n ]
  | Debruijn.Abs a -> [ Closure (compile a @ [ Return ]) ]
  | Debruijn.Let (a, b) -> compile a @ [ Let ] @ compile b @ [ Endlet ]
  | Debruijn.App (Var n, cond, [ then_expr; else_expr ]) when n = bIf ->
    compile cond @ [ If (compile then_expr, compile else_expr) ]
  | Debruijn.App (Var n, r, lst) when isBuiltin n ->
    compile r @ List.concat_map compile lst @ [ getBuiltinInstr n |> Option.get ]
  | Debruijn.App (l, r, lst) ->
    let args = r :: lst in
    compile l @ compile_lst compile args
  | Debruijn.Const n -> [ Const n ]
;;

type stack_item =
  | Atom of instr
  | Mem of instr list * stack_item list
[@@deriving show]

let rec eval1 (ins, env, stack) =
  match ins, env, stack with
  | Let :: ins, env, v :: stack -> eval ins (v :: env) stack
  | Endlet :: ins, _ :: env, stack -> eval ins env stack
  | Access n :: ins, env, stack -> eval ins env (List.nth env n :: stack)
  | Closure c_0 :: ins, env, stack -> eval ins env (Mem (c_0, env) :: stack)
  | Apply :: ins, env, v :: Mem (c_0, e_0) :: stack ->
    eval c_0 (v :: e_0) (Mem (ins, env) :: stack)
  | Return :: _, _, v :: Mem (c_0, e_0) :: stack -> eval c_0 e_0 (v :: stack)
  | Const n :: ins, env, stack -> eval ins env (Atom (Const n) :: stack)
  | Add :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (y + x)) :: stack)
  | Sub :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (y - x)) :: stack)
  | Mul :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (y * x)) :: stack)
  | Div :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (y / x)) :: stack)
  | Le :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if y < x then 1 else 0)) :: stack)
  | Ge :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if y > x then 1 else 0)) :: stack)
  | Leq :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if y <= x then 1 else 0)) :: stack)
  | Geq :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if y >= x then 1 else 0)) :: stack)
  | Ineq :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if y <> x then 1 else 0)) :: stack)
  | Eq :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if x = y then 1 else 0)) :: stack)
  | Or :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if y <> 0 || x <> 0 then 1 else 0)) :: stack)
  | And :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if y <> 0 && x <> 0 then 1 else 0)) :: stack)
  | If (then_expr, else_expr) :: ins, env, Atom (Const x) :: stack ->
    eval ((if x <> 0 then then_expr else else_expr) @ ins) env stack
  | _ -> Error "Unreachable by design"

and eval code env stack =
  match code with
  | [] -> Ok ([], env, stack)
  | _ ->
    Result.bind (eval1 (code, env, stack)) (fun (code, env, stack) -> eval code env stack)
;;

let eval code =
  match eval code [] [] with
  | Ok (_, _, Atom v :: _) -> Ok v
  | Ok _ -> Error "No atom on the stack"
  | Error msg -> Error msg
;;
