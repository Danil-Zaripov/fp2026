open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let number = take_while1 Char.Ascii.is_digit
let spaces = skip_while is_space
let varname = take_while1 Char.Ascii.is_letter

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

module StringSet = Set.Make (String)

let keywords = [ "fun"; "in"; "let"; "if"; "then"; "else" ] |> StringSet.of_list

let parse_var =
  varname
  >>= fun x ->
  if not @@ StringSet.mem x keywords
  then return @@ Ast.Var x
  else fail "Keyword used as ident"
;;

let parse_const = number >>= fun x -> return @@ Ast.Const (int_of_string x)

let parse_let expr =
  string "let" *> spaces *> varname
  >>= fun var ->
  spaces *> string "=" *> spaces *> expr
  >>= fun assign_part ->
  spaces *> string "in" *> spaces *> expr
  >>= fun in_part -> return @@ Ast.Let (var, assign_part, in_part)
;;

let%test _ =
  match
    parse_string
      ~consume:All
      (string "let" *> spaces *> parse_var <* spaces <* parse_const)
      "let xx  123"
  with
  | Ok v ->
    (match v with
     | Ast.Var x when x = "xx" -> true
     | _ -> false)
  | _ -> false
;;

let%test _ =
  match parse_string ~consume:All (parse_let parse_var) "let x = y in z" with
  | Ok v ->
    (match v with
     | Ast.Let (x, Ast.Var y, Ast.Var z) when x = "x" && y = "y" && z = "z" -> true
     | _ ->
       Format.eprintf "%a" (Printast.pp Format.pp_print_string) v;
       false)
  | Error e -> failwith e
;;

let parse_fun expr =
  string "fun" *> spaces *> varname
  >>= fun name ->
  spaces *> string "->" *> spaces *> expr >>= fun body -> return @@ Ast.Abs (name, body)
;;

let parse_if expr =
  string "if" *> spaces *> expr
  >>= fun cond ->
  spaces *> string "then" *> spaces *> expr
  >>= fun then_expr ->
  spaces *> string "else" *> spaces *> expr
  >>= fun else_expr -> return @@ Ast.If (cond, then_expr, else_expr)
;;

let parse_app atom =
  atom
  >>= fun main ->
  spaces *> atom
  >>= fun first ->
  many (spaces *> atom) >>= fun lst -> return @@ Ast.App (main, first, lst)
;;

let parse_paren_expr expr = string "(" *> spaces *> expr <* spaces <* string ")"

let parse_atom expr =
  conde
    [ parse_var
    ; parse_const
    ; parse_let expr
    ; parse_fun expr
    ; parse_if expr
    ; parse_paren_expr expr
    ]
;;

let parse_expr =
  fix (fun expr ->
    let atom = parse_atom expr in
    conde [ parse_app atom; atom ])
;;

let%test _ =
  match parse_string ~consume:All parse_expr "let x = z w in y" with
  | Ok v ->
    (match v with
     | Ast.Let (x, Ast.App (Ast.Var "z", Ast.Var "w", []), Ast.Var "y") when x = "x" ->
       true
     | _ -> false)
  | Error err -> failwith err
;;

let%test "if expr" =
  match parse_string ~consume:All parse_expr "if not x then y else z" with
  | Ok v ->
    (match v with
     | Ast.If (Ast.App (Ast.Var "not", Ast.Var "x", []), Ast.Var "y", Ast.Var "z") -> true
     | _ ->
       Format.eprintf "%a\n" Pprintast.pp v;
       false)
  | Error err -> failwith err
;;

let%test _ =
  match
    parse_string
      ~consume:All
      parse_expr
      "(fun x -> fun y -> x)(fun u -> u)((fun x -> x x)(fun x -> x x))"
  with
  | Ok v ->
    (match v with
     | _ -> true)
  | Error err -> failwith err
;;

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let varchar =
  satisfy (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let parse str =
  match Angstrom.parse_string parse_expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
