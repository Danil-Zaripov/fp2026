[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type lowered =
  | Var of string
  | Abs of string * lowered
  | App of lowered * lowered * lowered list
  | Let of string * lowered * lowered
  | Const of int
[@@deriving show]

module StringMap = Map.Make (String)

type env =
  { recs : string StringMap.t
  ; constrs : (int * int) StringMap.t (** (id, arity) map *)
  ; fresh_name_cnt : int
  }

let with_modified_recs f env = { env with recs = f env.recs }
let with_modified_constrs f env = { env with constrs = f env.constrs }
let get_new_constr_id env = StringMap.cardinal env.constrs

module State = struct
  type 'a t = env -> 'a * env

  let return (x : 'a) : 'a t = fun env -> x, env
  let pure = return

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun env ->
    let v, env' = m env in
    f v env'
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind
  let ( >> ) x y = bind x (fun _ -> y)
  let get : env t = fun env -> env, env
  let put new_env : unit t = fun _ -> (), new_env
  let change f : unit t = fun env -> (), f env

  let fresh_name =
    fun env ->
    ( Format.sprintf "$%d" env.fresh_name_cnt
    , { env with fresh_name_cnt = env.fresh_name_cnt + 1 } )
  ;;

  let local (change : env -> env) (m : 'a t) : 'a t =
    fun env ->
    let ret, _ = Fun.compose m change env in
    ret, env
  ;;

  let run ((x, _) : 'a * env) = x

  let rec mapM (f : 'a -> 'b t) (lst : 'a list) =
    match lst with
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = mapM f xs in
      return (y :: ys)
  ;;
end

let ( <|> ) x def =
  match x with
  | Some x -> x
  | None -> def
;;

let added_garbage = "#"
let recVar var = added_garbage ^ var
let if_check_name = added_garbage ^ "if"
let constructing_fun_name = added_garbage ^ "constr"
let check_constr_id_name = added_garbage ^ "check_constr_id"
let get_field_name = added_garbage ^ "get_field"
let err_name = added_garbage ^ "err"

let z_combinator =
  let addedGarbage = "^" in
  let rec mistify_vars = function
    | Ast.Abs (x, t) -> Ast.Abs (addedGarbage ^ x, mistify_vars t)
    | Ast.App (l, r, lst) ->
      Ast.App (mistify_vars l, mistify_vars r, List.map mistify_vars lst)
    | Ast.Var x -> Ast.Var (addedGarbage ^ x)
    | _ -> Ast.Var "#Unreachable"
  in
  Parser.parse "fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> (x x v)))"
  |> Result.get_ok
  |> mistify_vars
;;

let lower ast =
  let open State in
  let check_constr_id id value = App (Var check_constr_id_name, value, [ Const id ]) in
  let get_field value index = App (Var get_field_name, value, [ Const index ]) in
  let if_lowered cond then_expr else_expr =
    App (Var if_check_name, cond, [ then_expr; else_expr ])
  in
  let rec lower_if cond then_expr else_expr =
    let* cond = helper cond in
    let* then_expr = helper then_expr in
    let* else_expr = helper else_expr in
    return @@ if_lowered cond then_expr else_expr
  and multi_arg_function_decl param_list_reversed expr =
    match param_list_reversed with
    | [] -> helper expr
    | x :: xs ->
      let* fun_body = multi_arg_function_decl xs expr in
      return @@ Abs (x, fun_body)
  and helper = function
    | Ast.Const x -> return @@ Const x
    | Ast.Var x ->
      let* env = get in
      let x' = StringMap.find_opt x env.recs <|> x in
      return @@ Var x'
    | Ast.Abs (x, t) ->
      let* t = helper t in
      return @@ Abs (x, t)
    | Ast.App (l, r, xs) ->
      let* l = helper l in
      let* r = helper r in
      let* lst = mapM helper xs in
      return @@ App (l, r, lst)
    | Ast.Let (var, assign_part, in_part) ->
      let* assign_part = helper assign_part in
      let* in_part = helper in_part in
      return @@ Let (var, assign_part, in_part)
    | Ast.Letrec (var, assign_part, in_part) ->
      let* in_part = helper in_part in
      let new_var = recVar var in
      let* assign_part =
        local (with_modified_recs (StringMap.add var new_var)) (helper assign_part)
      in
      let* fix = helper z_combinator in
      return @@ Let (var, App (fix, Abs (new_var, assign_part), []), in_part)
    | Ast.If (cond, then_part, else_part) -> lower_if cond then_part else_part
    | Ast.Typedecl (_, [], following_expr) -> helper following_expr
    | Ast.Typedecl (name, (constr_name, arity) :: tl_contrs, f_expr) ->
      let* env = get in
      if env.constrs |> StringMap.mem constr_name
      then failwith "TODO: Redefinition?"
      else (
        let id = get_new_constr_id env in
        change (with_modified_constrs (StringMap.add constr_name (id, arity)))
        >>
        let* following_expr = helper @@ Ast.Typedecl (name, tl_contrs, f_expr) in
        let int_parameters = List.init arity Fun.id |> List.map string_of_int in
        let* apply_constr_fun =
          multi_arg_function_decl
            int_parameters
            (Ast.App
               ( Ast.Var constructing_fun_name
               , Ast.Const id
               , List.map (fun i -> Ast.Var i) int_parameters ))
        in
        return @@ Let (constr_name, apply_constr_fun, following_expr))
    | Ast.Constr (name, args) ->
      let* env = get in
      (match StringMap.find_opt name env.constrs with
       | None -> failwith "TODO: Undefined?"
       | Some (_, arity) when List.length args <> arity ->
         failwith "TODO: Arity mismatch?"
       | Some (id, _) ->
         let* lst = mapM helper args in
         return @@ App (Var constructing_fun_name, Const id, lst))
    | Ast.Match (mexpr, branches) ->
      (*
          let #1 = mexpr in #match #1 pat fallback
            where fallback = 
              #match #1 pat2 fallback2
                where fallback2 = #err
        *)
      let* env = get in
      let* mexpr = helper mexpr in
      let rec lower_pattern value success failure = function
        | Ast.PAny -> return @@ success
        | Ast.PValueName x -> return @@ Let (x, value, success)
        | Ast.PConstr (constr_name, args) ->
          let rec lower_fields value index success = function
            | [] -> return @@ success
            | x :: xs ->
              let* field_name = fresh_name in
              let* next_fields = lower_fields value (index + 1) success xs in
              let* fields = lower_pattern (Var field_name) next_fields failure x in
              return @@ Let (field_name, get_field value index, fields)
          in
          (match StringMap.find_opt constr_name env.constrs with
           | None -> failwith "TODO: Undefined?"
           | Some (_, arity) when List.length args <> arity ->
             failwith "TODO: Arity mismatch"
           | Some (id, _) ->
             let* fields = lower_fields value 0 success args in
             return @@ if_lowered (check_constr_id id value) fields failure)
      in
      let rec lower_branches = function
        | (pat, expr) :: branches ->
          let* success = helper expr in
          let* failure = lower_branches branches in
          let* branch = lower_pattern mexpr success failure pat in
          return @@ branch
        | [] -> return @@ Var err_name
      in
      let* branches = lower_branches branches in
      return @@ branches
  in
  run
  @@ helper ast { recs = StringMap.empty; constrs = StringMap.empty; fresh_name_cnt = 0 }
;;
