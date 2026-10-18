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

let%test "operator precedence" =
  (* (- (+ x ('*' x y)) (/ x y)) *)
  match parse "let f = fun x -> fun y -> x + x * y - x / y in f 3 4" with
  | Result.Ok
      (Ast.Let
         ( "f"
         , Ast.Abs
             ( "x"
             , Ast.Abs
                 ( "y"
                 , Ast.App
                     ( Ast.Var "-"
                     , Ast.App
                         ( Ast.Var "+"
                         , Ast.Var "x"
                         , [ Ast.App (Ast.Var "*", Ast.Var "x", [ Ast.Var "y" ]) ] )
                     , [ Ast.App (Ast.Var "/", Ast.Var "x", [ Ast.Var "y" ]) ] ) ) )
         , Ast.App (Ast.Var "f", Ast.Const 3, [ Ast.Const 4 ]) )) -> true
  | _ -> false
;;

let%test "parens" =
  match parse "1 + (n - 1) * (n + 1)" with
  | Result.Ok
      (Ast.App
         ( Ast.Var "+"
         , Ast.Const 1
         , [ Ast.App
               ( Ast.Var "*"
               , Ast.App (Ast.Var "-", Ast.Var "n", [ Ast.Const 1 ])
               , [ Ast.App (Ast.Var "+", Ast.Var "n", [ Ast.Const 1 ]) ] )
           ] )) -> true
  | _ -> false
;;

let%test "fib" =
  match parse "fib (n - 1) + fib (n - 2)" with
  | Result.Ok
      (Ast.App
         ( Ast.Var "+"
         , Ast.App (Ast.Var "fib", Ast.App (Ast.Var "-", Ast.Var "n", [ Ast.Const 1 ]), [])
         , [ Ast.App
               (Ast.Var "fib", Ast.App (Ast.Var "-", Ast.Var "n", [ Ast.Const 2 ]), [])
           ] )) -> true
  | _ -> false
;;
