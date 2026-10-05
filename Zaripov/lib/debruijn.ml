open Stdune.Monad

type deBruijn =
  | Const of int
  | Var of int
  | Abs of deBruijn
  | App of deBruijn * deBruijn * deBruijn list
  | Let of deBruijn * deBruijn

module StringMap = Map.Make (String)

type env = int StringMap.t

module Basis = struct
  type 'a t = env -> 'a * env

  let return (x : 'a) : 'a t = fun env -> x, env

  let bind (m : 'a t) ~(f : 'a -> 'b t) : 'b t =
    fun env ->
    let v, env' = m env in
    f v env'
  ;;
end

module State = struct
  type 'a t = 'a Basis.t

  include Stdune.Monad.Make (Basis)

  let run ((x, _) : 'a * env) : 'a = x
  let get : env t = fun env -> env, env
  let put (new_env : env) = fun _ -> (), new_env

  let rec mapM (f : 'a -> 'b t) (lst : 'a list) =
    let open O in
    match lst with
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = mapM f xs in
      return (y :: ys)
  ;;
end

let to_debruijn ast =
  let open State in
  let open O in
  let rec helper = function
    | Normal.Const x -> return @@ Const x
    | Normal.Abs (x, t) ->
      get
      >>| StringMap.map (fun x -> x + 1)
      >>| StringMap.add x 0
      >>= put
      >>>
      let* t = helper t in
      return @@ Abs t
    | Normal.App (l, r, xs) ->
      let* l = helper l in
      let* r = helper r in
      let* xs = mapM helper xs in
      return @@ App (l, r, xs)
    | Normal.Var x ->
      let* map = get in
      (match StringMap.find_opt x map with
       | Some x -> return @@ Var x
       | _ -> failwith "Undeclared name used")
    | Normal.Let (var, assign_part, in_part) ->
      get
      >>| StringMap.map (fun x -> x + 1)
      >>| StringMap.add var 0
      >>= put
      >>>
      let* assign_part = helper assign_part in
      let* in_part = helper in_part in
      return @@ Let (assign_part, in_part)
  in
  run @@ helper ast StringMap.empty
;;
