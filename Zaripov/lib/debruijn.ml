open Stdune.Monad

type de_bruijn =
  | Const of int
  | Var of int
  | Abs of de_bruijn
  | App of de_bruijn * de_bruijn * de_bruijn list
  | Let of de_bruijn * de_bruijn
[@@deriving show]

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

let builtinMap = function
  | "fix" -> Some (-1)
  | "+" -> Some (-2)
  | "#if" -> Some (-3)
  | "=" -> Some (-4)
  | "-" -> Some (-5)
  | _ -> None
;;

let defaultWith f = function
  | Some x -> Some x
  | None -> f ()
;;

let to_debruijn ast =
  let open State in
  let open O in
  let rec helper = function
    | Normal.Const x -> return @@ Const x
    | Normal.Abs (x, t) ->
      let* old_map = get in
      get
      >>| StringMap.map (fun x -> x + 1)
      >>| StringMap.add x 0
      >>= put
      >>>
      let* t = helper t in
      put old_map >>> return @@ Abs t
    | Normal.App (l, r, xs) ->
      let* env = get in
      let* l = helper l in
      put env
      >>>
      let* r = helper r in
      put env
      >>>
      let* xs = mapM (fun x -> put env >>> helper x) xs in
      put env >>> return @@ App (l, r, xs)
    | Normal.Var x ->
      let* map = get in
      (match defaultWith (fun () -> StringMap.find_opt x map) (builtinMap x) with
       | Some x -> return @@ Var x
       | _ -> failwith "Undeclared name used")
    | Normal.Let (var, assign_part, in_part) ->
      let* old_map = get in
      let* assign_part = helper assign_part in
      put old_map
      >>> get
      >>| StringMap.map (fun x -> x + 1)
      >>| StringMap.add var 0
      >>= put
      >>>
      let* in_part = helper in_part in
      put old_map >>> return @@ Let (assign_part, in_part)
  in
  run @@ helper ast StringMap.empty
;;

let straight_to_debruijn s =
  Parser.parse s |> Stdlib.Result.get_ok |> Normal.lower |> to_debruijn
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
