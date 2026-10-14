[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Normal

let straight_to_lower s = Parser.parse s |> Result.get_ok |> lower

let%test _ =
  match straight_to_lower "let rec fac = fun n -> mul n (fac (dec n)) in fac 5" with
  | Let
      ( "fac"
      , App
          ( x
          , Abs
              ( "#fac"
              , Abs
                  ( "n"
                  , App
                      ( Var "mul"
                      , Var "n"
                      , [ App (Var "#fac", App (Var "dec", Var "n", []), []) ] ) ) )
          , [] )
      , App (Var "fac", Const 5, []) )
    when x = lower z_combinator -> true
  | x ->
    Format.eprintf "%s" (show_lowered x);
    false
;;
