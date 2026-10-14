[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Parser

let%test _ =
  match parse "let x = y in z" with
  | Ok (Ast.Let (x, Ast.Var y, Ast.Var z)) when x = "x" && y = "y" && z = "z" -> true
  | _ -> false
;;

let%test "let rec and let" =
  match parse "let rec x = x in let y = x in z" with
  | Ok (Ast.Letrec ("x", Ast.Var "x", Ast.Let ("y", Ast.Var "x", Ast.Var "z"))) -> true
  | _ -> false
;;

let%test _ =
  match parse "let x = z w in y" with
  | Ok (Ast.Let (x, Ast.App (Ast.Var "z", Ast.Var "w", []), Ast.Var "y")) when x = "x" ->
    true
  | _ -> false
;;

let%test "if expr" =
  match parse "if not x then y else z" with
  | Ok (Ast.If (Ast.App (Ast.Var "not", Ast.Var "x", []), Ast.Var "y", Ast.Var "z")) ->
    true
  | _ -> true
;;

let%test _ =
  match parse "(fun x -> fun y -> x)(fun u -> u)((fun x -> x x)(fun x -> x x))" with
  | Ok _ -> true
  | _ -> false
;;

let%test _ =
  match parse "let rec fac = fun n -> mul n (fac (dec n)) in fac 5" with
  | Result.Ok _ -> true
  | _ -> false
;;

let%test _ =
  match parse "let inc = fun n -> n + 1 in inc 5" with
  | Result.Ok
      (Ast.Let
         ( "inc"
         , Ast.Abs ("n", App (Var "+", Var "n", [ Const 1 ]))
         , Ast.App (Var "inc", Const 5, []) )) -> true
  | Result.Ok ast ->
    Format.eprintf "%a" Pprintast.pp ast;
    false
  | Result.Error _ -> false
;;
