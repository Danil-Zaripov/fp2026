open Lambda_lib
open Parser

let%test _ =
  match parse "let x = y in z" with
  | Ok v ->
    (match v with
     | Ast.Let (x, Ast.Var y, Ast.Var z) when x = "x" && y = "y" && z = "z" -> true
     | _ ->
       Format.eprintf "%a" Pprintast.pp v;
       false)
  | Error (`Parsing_error e) -> failwith e
;;

let%test "let rec and let" =
  match parse "let rec x = x in let y = x in z" with
  | Ok v ->
    (match v with
     | Ast.Letrec ("x", Ast.Var "x", Ast.Let ("y", Ast.Var "x", Ast.Var "z")) -> true
     | tree ->
       Format.eprintf "%a" Pprintast.pp tree;
       false)
  | Error (`Parsing_error e) -> failwith e
;;

let%test _ =
  match parse "let x = z w in y" with
  | Ok v ->
    (match v with
     | Ast.Let (x, Ast.App (Ast.Var "z", Ast.Var "w", []), Ast.Var "y") when x = "x" ->
       true
     | _ -> false)
  | Error (`Parsing_error err) -> failwith err
;;

let%test "if expr" =
  match parse "if not x then y else z" with
  | Ok v ->
    (match v with
     | Ast.If (Ast.App (Ast.Var "not", Ast.Var "x", []), Ast.Var "y", Ast.Var "z") -> true
     | _ ->
       Format.eprintf "%a\n" Pprintast.pp v;
       false)
  | Error (`Parsing_error err) -> failwith err
;;

let%test _ =
  match parse "(fun x -> fun y -> x)(fun u -> u)((fun x -> x x)(fun x -> x x))" with
  | Ok v ->
    (match v with
     | _ -> true)
  | Error (`Parsing_error err) -> failwith err
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
  | Result.Error (`Parsing_error er) -> failwith er
;;
