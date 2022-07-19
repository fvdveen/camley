open Ast

type value = 
    | VoidObj
    | IntObj of int
    | FloatObj of float
    | BoolObj of bool
    | StringObj of string
    | ReturnObj of value
    | FnObj of var_table * string list * block

and bound_var = string * value

and var_table = bound_var list

and environment = stmt list * var_table

let rec string_of_object = function
    | VoidObj -> "VoidObj"
    | IntObj(i) -> Printf.sprintf "Int: %d" i
    | FloatObj(f) -> Printf.sprintf "Float: %f" f
    | BoolObj(b) -> Printf.sprintf "Bool: %b" b
    | StringObj(s) -> Printf.sprintf "String: \"%s\"" s
    | ReturnObj(v) -> "ReturnValue: " ^ string_of_object v
    | FnObj(_, _, _) -> "Fn"

let type_string_of_object = function
    | VoidObj -> "VoidObj"
    | IntObj(_) -> "Int" 
    | FloatObj(_) -> "Float" 
    | BoolObj(_) -> "Bool" 
    | StringObj(_) -> "String" 
    | ReturnObj(_) -> "ReturnValue"
    | FnObj(_) -> "Fn"

let is_truthy = function
    | VoidObj | BoolObj(false) -> false
    | _ -> true



let string_of_var_table = function
    | [] -> "Args: ()"
    | [(name, value)] -> Printf.sprintf "Args: (%s: %s)" name (string_of_object value)
    | (name, value) :: tail -> let rec helper base = function
        | [] -> base
        | (n, v) :: t -> helper (base ^ ", " ^ n ^ ": " ^ (string_of_object v)) t
    in ("Args: (" ^ name ^ ": " ^ string_of_object value ^ helper "" tail ^ ")")
