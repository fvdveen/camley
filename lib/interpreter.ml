open Ast
open Object
open Float

let insert k v lst = (k, v) :: lst

let rec insert_list lst = function
    | [] -> lst
    | [(k, v)] -> insert k v lst
    | kv :: t -> kv :: insert_list lst t

let rec lookup k = function
    | [] -> None
    | (k', v) :: t -> if k=k' then Some v else lookup k t

let bind value fn = match value with
    | Ok(v) -> fn v
    | Error(e) -> Error(e)

let rec repeat_string amount str = 
    if amount = 0 then "" else str ^ repeat_string (amount - 1) str

let unwrap_return_value = function
    | ReturnObj(v) -> v
    | x -> x

let rec eval_program = function
    | Program (stmts) -> eval_stmts (stmts, [])

and eval_stmts (stmts, env) =
    match stmts with
    | [] -> Ok(([], VoidObj))
    | [x] -> eval_stmt env x
    | h :: t -> bind (eval_stmt env h) (fun (env, _) -> eval_stmts (t, env))

and eval_stmt env stmt =
    match stmt with
    | Expr(_, expr) -> bind (eval_expr env expr) (fun value -> Ok(env, value))
    | Let(_, name, expr) -> bind (eval_expr env expr) (fun value -> Ok(insert name value env, VoidObj))
    | Return(_, expr) -> bind (eval_expr env expr) (fun value -> Ok(env, ReturnObj(value)))

and eval_expr env = function
    | Integer(_, i) -> Ok(IntObj(i))
    | Bool(_, b) -> Ok(BoolObj(b))
    | Float(_, f) -> Ok(FloatObj(f))
    | String(_, s) -> Ok(StringObj(s))
    | Fn(_, args, body) -> Ok(FnObj(env, args, body))
    | Identifier(loc, name) -> (
        match lookup name env with
        | Some value -> Ok(value)
        | None -> Error(Printf.sprintf "[%d:%d]: use of uninitialized variable: %s" loc.pos_lnum (loc.pos_cnum - loc.pos_bol + 1) name))
    | UnOp(_, op, expr) -> bind (eval_expr env expr) (fun value -> eval_un_op op value)
    | BinOp(_, op, left, right) -> 
        bind (eval_expr env left) (fun left ->
        bind (eval_expr env right) (fun right -> eval_bin_op op left right))
    | BlockExpr(_, b) -> eval_block env b
    | If(_, cond, cons, alt) -> bind (eval_expr env cond) (fun res -> 
        if is_truthy res 
            then eval_block env cons
            else match alt with
            | None -> Ok(VoidObj)
            | Some(b) -> eval_block env b)
    | FnCall(_, fn, args) -> bind (eval_expr env fn) (fun res -> match res with
        | FnObj(fn_env, arg_names, body) -> eval_fn_call env fn_env arg_names args body
        | _ -> Error(Printf.sprintf "cannot call %s object as function" (type_string_of_object res)))

and eval_un_op op value = match (op, value) with
    | (UnOpNeg, IntObj(value)) -> Ok(IntObj(-value))
    | (UnOpNeg, FloatObj(value)) -> Ok(FloatObj(-.value))
    | (UnOpNot, BoolObj(value)) -> Ok(BoolObj(not value))
    | _ -> Error(Printf.sprintf "operation %s not supported on type %s" (string_of_un_op op) (type_string_of_object value))

and eval_bin_op op left right = match (op, left, right) with 
    | (BinOpAnd, BoolObj(left), BoolObj(right)) -> Ok(BoolObj(left && right))
    | (BinOpOr, BoolObj(left), BoolObj(right)) -> Ok(BoolObj(left || right))
    | (BinOpMult, StringObj(s), IntObj(n)) -> Ok(StringObj(repeat_string n s))
    | (BinOpMult, IntObj(n), StringObj(s)) -> Ok(StringObj(repeat_string n s))
    | (BinOpPlus, StringObj(l), StringObj(r)) -> Ok(StringObj(l ^ r))
    | (BinOpEq, StringObj(l), StringObj(r)) -> Ok(BoolObj(l = r))
    | (BinOpNotEq, StringObj(l), StringObj(r)) -> Ok(BoolObj(l != r))
    | (op, IntObj(l), IntObj(r)) -> execute_int_bin_op op l r
    | (op, IntObj(l), FloatObj(r)) -> (let value = execute_float_bin_op op (float_of_int l) r in match value with
        | Some(value) -> Ok(value)
        | None -> Error(Printf.sprintf "operation %s not supported on types %s, %s" (string_of_bin_op op) (type_string_of_object left) (type_string_of_object right)))
    | (op, FloatObj(l), IntObj(r)) -> (let value = execute_float_bin_op op l (float_of_int r) in match value with
        | Some(value) -> Ok(value)
        | None -> Error(Printf.sprintf "operation %s not supported on types %s, %s" (string_of_bin_op op) (type_string_of_object left) (type_string_of_object right)))
    | (op, FloatObj(l), FloatObj(r)) -> (let value = execute_float_bin_op op l r in match value with
        | Some(value) -> Ok(value)
        | None -> Error(Printf.sprintf "operation %s not supported on types %s, %s" (string_of_bin_op op) (type_string_of_object left) (type_string_of_object right)))
    | _ -> Error(Printf.sprintf "operation %s not supported on types %s, %s" (string_of_bin_op op) (type_string_of_object left) (type_string_of_object right))

and eval_block env = function
    | Block(_, stmts) -> (
        let rec stmts_helper env = function
        | [] -> Ok(VoidObj)
        | [x] -> bind (eval_stmt env x) (fun (_, res) -> Ok(res))
        | h :: t -> bind (eval_stmt env h) (fun (new_env, res) -> 
            match res with
            | ReturnObj(v) -> Ok(ReturnObj(v))
            | _ -> stmts_helper new_env t)
        in stmts_helper env stmts)

and execute_int_bin_op op left right = match op with
    | BinOpPlus -> Ok(IntObj(left + right))
    | BinOpMinus -> Ok(IntObj(left - right))
    | BinOpMult -> Ok(IntObj(left * right))
    | BinOpDiv -> Ok(IntObj(left / right))
    | BinOpRem -> Ok(IntObj(left mod right))
    | BinOpEq -> Ok(BoolObj(left = right))
    | BinOpNotEq -> Ok(BoolObj(left != right))
    | BinOpLessThan -> Ok(BoolObj(left < right))
    | BinOpLessThanEq -> Ok(BoolObj(left <= right))
    | BinOpGreaterThan -> Ok(BoolObj(left > right))
    | BinOpGreaterThanEq -> Ok(BoolObj(left >= right))
    | BinOpAnd | BinOpOr -> Error(Printf.sprintf "operation %s not supported on types Int, Int" (string_of_bin_op op))

and execute_float_bin_op op left right = match op with
    | BinOpPlus -> Some(FloatObj(left +. right))
    | BinOpMinus -> Some(FloatObj(left -. right))
    | BinOpMult -> Some(FloatObj(left *. right))
    | BinOpDiv -> Some(FloatObj(left /. right))
    | BinOpRem -> Some(FloatObj(rem left right))
    | BinOpEq -> Some(BoolObj(left = right))
    | BinOpNotEq -> Some(BoolObj(left != right))
    | BinOpLessThan -> Some(BoolObj(left < right))
    | BinOpLessThanEq -> Some(BoolObj(left <= right))
    | BinOpGreaterThan -> Some(BoolObj(left > right))
    | BinOpGreaterThanEq -> Some(BoolObj(left >= right))
    | BinOpAnd | BinOpOr -> None

and eval_args env arg_names args =
    let rec helper env lst arg_names args = 
    match arg_names, args with
    | [], [] -> Ok(lst)
    | arg_name :: arg_names, arg :: args -> bind (eval_expr env arg) (fun res -> 
        helper env ((arg_name, res) :: lst) arg_names args)
    | _ -> Error("function argument list does not match")
    in helper env [] arg_names args

and eval_fn_call env fn_env arg_names args body =
    match eval_args env arg_names args with
    | Error(e) -> Error(e)
    | Ok(args) -> bind (eval_block (insert_list fn_env args) body) (fun ret -> Ok(unwrap_return_value ret))