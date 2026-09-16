open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let varname =
  satisfy (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let%test _ =
  let p1 = char 'a' *> char 'b' in
  match parse_string p1 ~consume:All "ab" with
  | Ok _ -> true
  | _ -> false
;;

let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ char '(' *> pack.apps pack <* char ')' <?> "Parentheses expected"
        ; ((string "λ" <|> string "\\") *> spaces *> varname
           <* spaces
           <* (return () <* char '.' <|> string "->" *> return ())
           >>= fun var ->
           pack.apps pack >>= fun b -> return (Ast.Abs (String.make 1 var, b)))
        ; (varname <* spaces >>= fun c -> return (Ast.Var (String.make 1 c)))
        ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r, [])) x xs
  in
  { single; apps }
;;

let parse str =
  match
    Angstrom.parse_string (parse_lam.apps parse_lam) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
