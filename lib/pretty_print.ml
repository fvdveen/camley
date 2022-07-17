open Ast

let indent_space = "   "

let rec pprint_expr ~indent expr = 
    let print_expr = Printf.printf "%sExpr: %s@.\n" indent in
    let new_indent = indent_space ^ indent in
    match expr with
    | Integer(_, i) -> print_expr (Format.sprintf "Int: %d" i)
    | Float(_, f) -> print_expr (Format.sprintf "Float: %f" f)
    | String(_, s) -> print_expr (Format.sprintf "String: \"%s\"" s)
    | Identifier(_, id) -> print_expr (Format.sprintf "Identifier: %s" id)
    | Bool(_, b) -> print_expr (Format.sprintf "Bool: %b" b)
    | UnOp(_, op, expr) -> 
        print_expr (Format.sprintf "UnOp: %s" (string_of_un_op op));
        pprint_expr ~indent:new_indent expr
    | BinOp(_, op, e1, e2) ->
        print_expr (Format.sprintf "BinOp: %s" (string_of_bin_op op));
        pprint_expr ~indent:new_indent e1;
        pprint_expr ~indent:new_indent e2
    | FnCall(_, f, args) ->
        print_expr "Function Call:";
        Printf.printf "%sFunction:@.\n" new_indent;
        pprint_expr ~indent:(indent_space ^ new_indent) f;
        pprint_args ~indent:new_indent args
    | Fn(_, args, body) -> 
        print_expr "Function:";
        Printf.printf "%sArgs:@.\n" new_indent;
        (match args with
        | [] -> Printf.printf "%s()@.\n" (indent_space ^ new_indent)
        | _ -> List.iter (Printf.printf "%sArg: %s@.\n" (indent_space ^new_indent)) args);
        pprint_block ~indent:new_indent "Body" body
    | BlockExpr(_, b) ->
        pprint_block ~indent:indent "BlockExpr" b
    | If(_, cond, cons, alt) -> 
        print_expr "If:";
        Printf.printf "%sCondition:@.\n" new_indent;
        pprint_expr ~indent:(indent_space ^ new_indent) cond;
        pprint_block ~indent:new_indent "Consequence" cons;
        (match alt with
        | Some b -> pprint_block ~indent:new_indent "Alternative" b
        | None -> Printf.printf "%sAlternative:@.\n%sNone@.\n" new_indent (indent_space ^ new_indent))
    

and pprint_block ~indent name block =
    let new_indent = indent_space ^ indent in
    match block with
    | Block(_, stmts) ->
        Printf.printf "%s%s:@.\n" indent name;
        List.iter (pprint_stmt ~indent:new_indent) stmts;

and pprint_args ~indent = function
  | []   ->  Printf.printf "%s()@.\n" indent
  | args -> List.iter (pprint_expr ~indent) args

and pprint_stmt ~indent stmt = 
    let print_stmt = Printf.printf "%sStmt: %s@.\n" indent in
    let new_indent = indent_space ^ indent in
    match stmt with
    | Let (_, name, expr) -> 
        print_stmt (Format.sprintf "Let %s:" name);
        pprint_expr ~indent:new_indent expr
    | Return(_, value) ->
        print_stmt "Return:";
        pprint_expr ~indent:new_indent value
    | Expr(_, expr) ->
        print_stmt "Expr:";
        pprint_expr ~indent:new_indent expr

let pprint_program (Program(statements)) = 
    print_endline "Program@.";
    let indent = "└──" in
    List.iter (pprint_stmt ~indent) statements;
    ()