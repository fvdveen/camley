open Ast

type value = 
    | VoidObj
    | IntObj of int
    | FloatObj of float
    | BoolObj of bool
    | StringObj of string

let string_of_object = function
    | VoidObj -> "VoidObj"
    | IntObj(i) -> Printf.sprintf "Int: %d" i
    | FloatObj(f) -> Printf.sprintf "Float: %f" f
    | BoolObj(b) -> Printf.sprintf "Bool: %b" b
    | StringObj(s) -> Printf.sprintf "String: \"%s\"" s

let type_string_of_object = function
    | VoidObj -> "VoidObj"
    | IntObj(_) -> Printf.sprintf "Int" 
    | FloatObj(_) -> Printf.sprintf "Float" 
    | BoolObj(_) -> Printf.sprintf "Bool" 
    | StringObj(_) -> Printf.sprintf "String" 

let is_truthy = function
    | VoidObj | BoolObj(false) -> false
    | IntObj(_) | FloatObj(_) | BoolObj(true) | StringObj(_) -> true

type bound_var = string * value

type var_table = bound_var list

type environment = stmt list * var_table