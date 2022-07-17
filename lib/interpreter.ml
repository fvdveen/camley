open Ast
open Object

let insert k v lst = (k, v) :: lst
let rec lookup k = function
    | [] -> None
    | (k', v) :: t -> if k=k' then Some v else lookup k t

let bind value fn = match value with
    | Ok(v) -> fn v
    | Error(e) -> Error(e)

let rec eval_program = function
    | Program (stmts) -> eval_stmts [] stmts

and eval_stmts env stmts =
    match stmts with
    | [] -> Ok(([], VoidObj))
    | [x] -> eval_stmt env x
    | h :: t -> bind (eval_stmt env h) (fun (env, _) -> eval_stmts env t)

and eval_stmt env stmt =
    match stmt with
    | Expr(_, expr) -> bind (eval_expr env expr) (fun value -> Ok((env, value)))
    | Let(_, name, expr) -> bind (eval_expr env expr) (fun value -> Ok((insert name value env, VoidObj)))
    | _ -> Error("unimplemented")

and eval_expr env = function
    | Integer(_, i) -> Ok(IntObj(i))
    | Identifier(_, name) -> (match lookup name env with
        | Some value -> Ok(value)
        | None -> Error(Printf.sprintf "use of uninitialized variable: %s" name))
    | _ -> Error("unimplmented")