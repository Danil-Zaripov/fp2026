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

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

module StringSet = Set.Make (String)

let keywords = [ "fun"; "in"; "let"; "if"; "then"; "else"; "rec" ] |> StringSet.of_list

let parse_var =
  varname
  >>= fun x ->
  if not @@ StringSet.mem x keywords
  then return @@ Ast.Var x
  else fail "Keyword used as ident"
;;

let parse_const = number >>= fun x -> return @@ Ast.Const (int_of_string x)

let parse_let_nonrec expr =
  string "let" *> spaces *> varname
  >>= fun var ->
  spaces *> string "=" *> spaces *> expr
  >>= fun assign_part ->
  spaces *> string "in" *> spaces *> expr
  >>= fun in_part -> return @@ Ast.Let (var, assign_part, in_part)
;;

let parse_letrec expr =
  let* var = string "let" *> spaces *> string "rec" *> spaces *> varname in
  let* assign_part = spaces *> string "=" *> spaces *> expr in
  let* in_part = spaces *> string "in" *> spaces *> expr in
  return @@ Ast.Letrec (var, assign_part, in_part)
;;

let parse_let expr = parse_let_nonrec expr <|> parse_letrec expr

let parse_bin s atom =
  let* l = atom <* spaces in
  string s
  *> spaces
  *>
  let* r = atom in
  return @@ Ast.App (Var s, l, [ r ])
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
    conde
      [ parse_app atom; parse_bin "+" atom; parse_bin "=" atom; parse_bin "-" atom; atom ])
;;

type error = [ `Parsing_error of string ]

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
