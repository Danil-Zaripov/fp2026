open Ast

type lowered =
  | Var of string
  | Abs of string * lowered
  | App of lowered * lowered * lowered list
  | Let of string * lowered * lowered
  | Const of int

type env_mode =
  | Retrieve
  | Put

let lower env =
  let retrieve = env Retrieve in
  let put = env Put in
  function
  | Ast.Var x -> Var (retrieve x)
  | Ast.Abs (x, t) -> 
;;
