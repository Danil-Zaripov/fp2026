[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Debruijn

let straight_to_debruijn s =
  Parser.parse s |> Stdlib.Result.get_ok |> Normal.lower |> to_debruijn |> Result.get_ok
;;

let%test _ =
  match straight_to_debruijn "fun x -> fun y -> x y" with
  | Abs (Abs (App (Var 1, Var 0, []))) -> true
  | _ -> false
;;

let%test _ =
  match straight_to_debruijn "let id = fun x -> x in id 5" with
  | Let (Abs (Var 0), App (Var 0, Const 5, [])) -> true
  | _ -> false
;;
