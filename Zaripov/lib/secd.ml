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
  | Eq
  | If of instr list * instr list
[@@deriving show]

let rec compile =
  let compile_lst f = List.concat_map (fun x -> f x @ [ Apply ]) in
  function
  | Debruijn.Var n -> [ Access n ]
  | Debruijn.Abs a -> [ Closure (compile a @ [ Return ]) ]
  | Debruijn.Let (a, b) -> compile a @ [ Let ] @ compile b @ [ Endlet ]
  | Debruijn.App (Var n, r, lst) when n = bAdd ->
    compile r @ List.concat_map compile lst @ [ Add ]
  | Debruijn.App (Var n, cond, [ then_expr; else_expr ]) when n = bIf ->
    compile cond @ [ If (compile then_expr, compile else_expr) ]
  | Debruijn.App (Var n, r, lst) when n = bEq ->
    compile r @ List.concat_map compile lst @ [ Eq ]
  | Debruijn.App (Var n, r, lst) when n = bSub ->
    compile r @ List.concat_map compile lst @ [ Sub ]
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
    eval ins env (Atom (Const (x + y)) :: stack)
  | Sub :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (y - x)) :: stack)
  | Eq :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if x = y then 1 else 0)) :: stack)
  | If (then_expr, else_expr) :: ins, env, Atom (Const x) :: stack ->
    eval ((if x <> 0 then then_expr else else_expr) @ ins) env stack
  | _ ->
    Format.eprintf
      "%s\n%s\n%s\n"
      ([%show: instr list] ins)
      ([%show: stack_item list] env)
      ([%show: stack_item list] stack);
    Error "Unreachable by design"

and eval code env stack =
  match code with
  | [] -> Ok ([], env, stack)
  | _ ->
    Result.bind (eval1 (code, env, stack)) (fun (code, env, stack) -> eval code env stack)
;;

let eval code =
  match eval code [] [] with
  | Ok (_, _, Atom v :: _) -> Ok v
  | Ok (ins, env, stack) ->
    Format.eprintf
      "%s\n%s\n%s\n"
      ([%show: instr list] ins)
      ([%show: stack_item list] env)
      ([%show: stack_item list] stack);
    Error "Nothing on the stack"
  | Error msg -> Error msg
;;
