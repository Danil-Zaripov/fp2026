[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Secd

let straight_to_instrs s =
  let instrs =
    Parser.parse s
    |> Result.get_ok
    |> Normal.lower
    |> Debruijn.to_debruijn
    |> Result.get_ok
    |> compile
  in
  instrs
;;

let straight_to_eval = Fun.compose Result.get_ok (Fun.compose eval straight_to_instrs)

let%test _ =
  match straight_to_eval "let x = 5 in x" with
  | Const 5 -> true
  | _ -> false
;;

let%test _ =
  match straight_to_eval "let id = fun x -> x in id 5" with
  | Const 5 -> true
  | _ -> false
;;

let%test _ =
  match straight_to_eval "let inc = fun n -> n + 1 in inc 4" with
  | Const 5 -> true
  | _ -> false
;;

let%test _ =
  match straight_to_eval "let c = fun n -> if n then 5 else 10 in c (0 + 1)" with
  | Const 5 -> true
  | _ -> false
;;

let%test _ =
  match
    straight_to_eval
      "let rec sum = fun n -> if n = 0 then 0 else (sum (n - 1)) + n in sum 5"
  with
  | Const 15 -> true
  | _ -> false
;;

let%test _ =
  let txt =
    "let rec fib = fun n -> if n = 0 then 0 else if n = 1 then 1 else (fib (n - 1)) + \
     (fib (n - 2)) in fib 10"
  in
  match straight_to_eval txt with
  | Const 55 -> true
  | x ->
    Format.eprintf "%s" (show_instr x);
    false
;;
