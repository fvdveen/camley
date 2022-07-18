open Ast
open Object
open Float

let insert k v lst = (k, v) :: lst
let rec lookup k = function
    | [] -> None
    | (k', v) :: t -> if k=k' then Some v else lookup k t

let bind value fn = match value with
    | Ok(v) -> fn v
    | Error(e) -> Error(e)

let rec repeat_string amount str = 
    if amount = 0 then "" else str ^ repeat_string (amount - 1) str

let rec eval_program = function
    | Program (stmts) -> eval_stmts (stmts, [])

and eval_stmts (stmts, env) =
    match stmts with
    | [] -> Ok(([], VoidObj))
    | [x] -> eval_stmt env x
    | h :: t -> bind (eval_stmt env h) (fun (env, _) -> eval_stmts (t, env))

and eval_stmt env stmt =
    match stmt with
    | Expr(_, expr) -> bind (eval_expr env expr) (fun value -> Ok((env, value)))
    | Let(_, name, expr) -> bind (eval_expr env expr) (fun value -> Ok((insert name value env, VoidObj)))
    | _ -> Error("unimplemented")

and eval_expr env = function
    | Integer(_, i) -> Ok(IntObj(i))
    | Bool(_, b) -> Ok(BoolObj(b))
    | Float(_, f) -> Ok(FloatObj(f))
    | String(_, s) -> Ok(StringObj(s))
    | Identifier(_, name) -> (match lookup name env with
        | Some value -> Ok(value)
        | None -> Error(Printf.sprintf "use of uninitialized variable: %s" name))
    | UnOp(_, op, expr) -> bind (eval_expr env expr) (fun value -> eval_un_op op value)
    | BinOp(_, op, left, right) -> bind (eval_expr env left) (fun left ->
        bind (eval_expr env right) (fun right -> eval_bin_op op left right))
    | _ -> Error("unimplmented")

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

