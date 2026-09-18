open QCheck
open QCheck.Gen
open Lambda_lib

(* size >= 5 to never create a keyword *)
let gen_name = string_size_of (int_range 5 10) (char_range 'a' 'z')
let gen_var = gen_name >|= fun s -> Ast.Var s
let gen_const = return @@ Ast.Const 1
let gen_simple = oneof_weighted [ 4, gen_var; 1, gen_const ]

let gen_let expr size =
  gen_name
  >>= fun name ->
  expr size
  >>= fun assign_part -> expr size >|= fun in_part -> Ast.Let (name, assign_part, in_part)
;;

let gen_if expr size =
  expr size
  >>= fun cond ->
  expr size
  >>= fun then_expr -> expr size >|= fun else_expr -> Ast.If (cond, then_expr, else_expr)
;;

let gen_abs expr size =
  gen_name >>= fun name -> expr size >|= fun body -> Ast.Abs (name, body)
;;

let gen_atom expr size =
  if size < 1
  then gen_simple
  else (
    let new_size = size - 1 in
    oneof_weighted
      [ 5, gen_let expr new_size; 2, gen_if expr new_size; 3, gen_abs expr new_size ])
;;

let gen_expr =
  fix (fun expr ->
    fun size ->
    list_size (int_range 2 3) (gen_atom expr size)
    >|= function
    | l :: r :: lst -> Ast.App (l, r, lst)
    | _ -> failwith "Unreachable")
;;

let get_ast_program ast = Format.asprintf "%a" Pprintast.pp ast
let parse_optimistically = Fun.compose Result.get_ok Parser.parse
let ast_arb ast_gen = make ~print:get_ast_program ast_gen

let test_var =
  Test.make
    ~count:100
    (ast_arb (gen_expr 3))
    (fun ast ->
       let str = get_ast_program ast in
       let after_ast = parse_optimistically str in
       ast = after_ast)
;;

QCheck_runner.run_tests_main [ test_var ]
