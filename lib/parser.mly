%{
	open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token LPAREN
%token RPAREN 
%token LBRACE 
%token RBRACE 
%token LBRACK
%token RBRACK
%token LANGLE
%token RANGLE
%token COMMA 
%token PERIOD
%token COLON 
%token SEMI
%token EXCLAMATION_MARK
%token ASSIGN
%token PLUS
%token MINUS
%token MULT
%token DIV
%token REM
%token AND
%token OR
%token EOF
%token EQUAL
%token NOTEQUAL
%token LANGLEEQUAL
%token RANGLEEQUAL

%token TRUE
%token FALSE

%token FN
%token IF
%token ELSE
%token RETURN
%token LET

%right    ASSIGN
%left     AND OR  
%left     EQUAL NOTEQUAL
%left     LANGLE RANGLE LANGLEEQUAL RANGLEEQUAL
%left     PLUS MINUS
%left     MULT DIV REM
%nonassoc EXCLAMATION_MARK

%start program

%type <Ast.program> program

%type <Ast.stmt> stmt
%type <Ast.expr> expr

%%

program:
    | statements=stmt* EOF { Program statements }
    ;

stmt:
    | LET name=ID ASSIGN value=expr SEMI { Let($startpos, name, value) }
	| RETURN value=expr SEMI { Return($startpos, value) }
	| e=expr SEMI { Expr($startpos, e) }
    ;

expr:
	| TRUE { Bool($startpos, true) }
	| FALSE { Bool($startpos, false) }
    | i=INT { Integer($startpos, i) }
	| f=FLOAT { Float($startpos, f) }
	| s=STRING { String($startpos, s) }
	| id=ID { Identifier($startpos, id) }
	| e=delimited(LPAREN, expr, RPAREN) { e }
	| op=un_op e=expr { UnOp($startpos, op, e) }
	| e1=expr op=bin_op e2=expr { BinOp($startpos, op, e1, e2) }
	| f=expr args=delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { FnCall($startpos, f, args) }
	| i=cond { i }
	| b=block { BlockExpr($startpos, b) }
	| FN args=delimited(LPAREN, separated_list(COMMA, ID), RPAREN) b=block { Fn($startpos, args, b) }
    ;

block: 
	| LBRACE stmts=stmt* SEMI? RBRACE { Block($startpos, stmts) }

cond: 
	| IF cond=delimited(LPAREN, expr, RPAREN) cons=block alt=cond_alt? { If($startpos, cond, cons, alt) }

cond_alt:
	| ELSE body=block { body }

%inline un_op:
| EXCLAMATION_MARK {UnOpNot}
| MINUS {UnOpNeg}

%inline bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULT { BinOpMult }
| DIV { BinOpDiv } 
| REM { BinOpRem }
| LANGLE { BinOpLessThan }
| LANGLEEQUAL { BinOpLessThanEq }
| RANGLE { BinOpGreaterThan }
| RANGLEEQUAL{ BinOpGreaterThanEq }
| AND {BinOpAnd}
| OR {BinOpOr}
| EQUAL EQUAL {BinOpEq}
| NOTEQUAL {BinOpNotEq}