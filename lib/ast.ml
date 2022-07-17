type loc = Lexing.position

type bin_op =
    | BinOpPlus
    | BinOpMinus
    | BinOpMult
    | BinOpDiv
    | BinOpRem
    | BinOpLessThan
    | BinOpLessThanEq
    | BinOpGreaterThan
    | BinOpGreaterThanEq
    | BinOpAnd
    | BinOpOr
    | BinOpEq
    | BinOpNotEq

let string_of_bin_op = function
    | BinOpPlus          -> "+"
    | BinOpMinus         -> "-"
    | BinOpMult          -> "*"
    | BinOpDiv           -> "/"
    | BinOpRem           -> "%"
    | BinOpLessThan      -> "<"
    | BinOpLessThanEq    -> "<="
    | BinOpGreaterThan   -> ">"
    | BinOpGreaterThanEq -> ">="
    | BinOpAnd           -> "&&"
    | BinOpOr            -> "||"
    | BinOpEq            -> "=="
    | BinOpNotEq         -> "!="

type un_op = UnOpNot | UnOpNeg

let string_of_un_op = function UnOpNot -> "!" | UnOpNeg -> "-"

type expr =
	| Identifier of loc * string
    | Integer of loc * int
	| Float of loc * float
	| String of loc * string
    | Bool of loc * bool
	| BinOp of loc * bin_op * expr * expr
	| UnOp of loc * un_op * expr
	| FnCall of loc * expr * expr list
	| Fn of loc * string list * block
	| If of loc * expr * block * block option
	| BlockExpr of loc * block


and block = Block of loc * stmt list

and stmt = 
	| Let of loc * string * expr
	| Return of loc * expr
	| Expr of loc * expr

type program = Program of stmt list