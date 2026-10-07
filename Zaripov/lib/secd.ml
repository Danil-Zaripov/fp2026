type instr =
  | Access of int
  | Closure of instr list
  | Let
  | Endlet
  | Apply
  | Return
  | Const of int
[@@deriving show]

let predefined = function
  | ".const" -> -1
  | _ -> failwith "undefined"
;;

let rec compile = function
  | Debruijn.Var n -> [ Access n ]
  | Debruijn.Abs a -> [ Closure (compile a @ [ Return ]) ]
  | Debruijn.Let (a, b) -> compile a @ [ Let ] @ compile b @ [ Endlet ]
  | Debruijn.App (l, r, lst) ->
    compile l @ compile r @ (Fun.compose List.concat (List.map compile)) lst @ [ Apply ]
  | Debruijn.Const n -> [ Const n ]
;;

type stack_item =
  | Atom of instr
  | Mem of instr list * instr list
[@@deriving show]

let rec eval1 (ins, env, stack) =
  match ins, env, stack with
  | Let :: ins, env, Atom v :: stack -> eval ins (v :: env) stack
  | Endlet :: ins, _ :: env, stack -> eval ins env stack
  | Access n :: ins, env, stack -> eval ins env (Atom (List.nth env n) :: stack)
  | Closure c_0 :: ins, env, stack -> eval ins env (Mem (c_0, env) :: stack)
  | Apply :: ins, env, Atom v :: Mem (c_0, e_0) :: stack ->
    eval c_0 (v :: e_0) (Mem (ins, env) :: stack)
  | Return :: _, _, v :: Mem (c_0, e_0) :: stack -> eval c_0 e_0 (v :: stack)
  | Const n :: ins, env, stack -> eval ins env (Atom (Const n) :: stack)
  | _ ->
    Format.eprintf
      "%s\n%s\n%s\n"
      ([%show: instr list] ins)
      ([%show: instr list] env)
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
  | _ -> failwith "Nothing on the stack"
;;

let straight_to_instrs s =
  Parser.parse s |> Result.get_ok |> Normal.lower |> Debruijn.to_debruijn |> compile
;;

let straight_to_eval = Fun.compose eval_result straight_to_instrs

let%test _ =
  match straight_to_eval "let x = 5 in x" with
  | Const 5 -> true
  | _ -> false
;;
