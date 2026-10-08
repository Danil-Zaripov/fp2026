type instr =
  | Access of int
  | Closure of instr list
  | Let
  | Endlet
  | Apply
  | Return
  | Const of int
  | Add
  | Sub
  | Eq
  | If of instr list * instr list
[@@deriving show]

let predefined = function
  | ".const" -> -1
  | _ -> failwith "undefined"
;;

let builtinMap = function
  | -2 -> [ Add ]
  | _ -> failwith "Undefined"
;;

let isBuiltin n = n < 0
let add_id = -2

let rec compile =
  let compile_lst f = List.concat_map (fun x -> f x @ [ Apply ]) in
  function
  | Debruijn.Var n -> if n >= 0 then [ Access n ] else builtinMap n
  | Debruijn.Abs a -> [ Closure (compile a @ [ Return ]) ]
  | Debruijn.Let (a, b) -> compile a @ [ Let ] @ compile b @ [ Endlet ]
  | Debruijn.App (Var -1, r, lst) -> failwith "Unreachable"
  | Debruijn.App (Var -2, r, lst) -> compile r @ List.concat_map compile lst @ [ Add ]
  | Debruijn.App (Var -3, cond, [ then_expr; else_expr ]) ->
    compile cond @ [ If (compile then_expr, compile else_expr) ]
  | Debruijn.App (Var -4, r, lst) -> compile r @ List.concat_map compile lst @ [ Eq ]
  | Debruijn.App (Var -5, r, lst) -> compile r @ List.concat_map compile lst @ [ Sub ]
  | Debruijn.App (l, r, lst) ->
    let args = r :: lst in
    compile l @ compile_lst compile args
  | Debruijn.Const n -> [ Const n ]
;;

type stack_item =
  | Atom of instr
  | Mem of instr list * stack_item list
[@@deriving show]

let rec eval1 (ins, env, stack) =
  match ins, env, stack with
  | Let :: ins, env, v :: stack -> eval ins (v :: env) stack
  | Endlet :: ins, _ :: env, stack -> eval ins env stack
  | Access n :: ins, env, stack -> eval ins env (List.nth env n :: stack)
  | Closure c_0 :: ins, env, stack -> eval ins env (Mem (c_0, env) :: stack)
  | Apply :: ins, env, v :: Mem (c_0, e_0) :: stack ->
    eval c_0 (v :: e_0) (Mem (ins, env) :: stack)
  | Return :: _, _, v :: Mem (c_0, e_0) :: stack -> eval c_0 e_0 (v :: stack)
  | Const n :: ins, env, stack -> eval ins env (Atom (Const n) :: stack)
  | Add :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (x + y)) :: stack)
  | Sub :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (y - x)) :: stack)
  | Eq :: ins, env, Atom (Const x) :: Atom (Const y) :: stack ->
    eval ins env (Atom (Const (if x = y then 1 else 0)) :: stack)
  | If (then_expr, else_expr) :: ins, env, Atom (Const x) :: stack ->
    eval ((if x <> 0 then then_expr else else_expr) @ ins) env stack
  | _ ->
    Format.eprintf
      "%s\n%s\n%s\n"
      ([%show: instr list] ins)
      ([%show: stack_item list] env)
      ([%show: stack_item list] stack);
    failwith "Unreachable by design"

and eval code env stack =
  match code with
  | [] -> [], env, stack
  | _ ->
    let code, env, stack = eval1 (code, env, stack) in
    eval code env stack
;;

let eval_result code =
  match eval code [] [] with
  | _, _, Atom v :: _ -> v
  | ins, env, stack ->
    Format.eprintf
      "%s\n%s\n%s\n"
      ([%show: instr list] ins)
      ([%show: stack_item list] env)
      ([%show: stack_item list] stack);
    failwith "Nothing on the stack"
;;

let straight_to_instrs s =
  let instrs =
    Parser.parse s |> Result.get_ok |> Normal.lower |> Debruijn.to_debruijn |> compile
  in
  instrs
;;

let straight_to_eval = Fun.compose eval_result straight_to_instrs

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
