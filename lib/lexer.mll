{
	open Lexing
	open Parser

	exception SyntaxError of string

	let next_line lexbuf = 
		let pos = lexbuf.lex_curr_p in 
		lexbuf.lex_curr_p <- 
		{ pos with pos_bol  = lexbuf.lex_curr_pos;
			       pos_lnum = pos.pos_lnum + 1
		}
}

(* helper regexes *)
let digit = [ '0' - '9' ]
let alpha = [ 'a' - 'z' 'A' - 'Z' ]

let int = '-'? digit+
let float = '-'? digit+ '.' digit*
let id = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token = parse 
(* simple tokens *)
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "{" { LBRACE }
	| "}" { RBRACE }
	| "[" { LBRACK }
	| "]" { RBRACK }
	| "<" { LANGLE }
	| ">" { RANGLE }
	| ":" { COLON }
	| ";" { SEMI }
	| "," { COMMA }
	| "." { PERIOD }
	| "!" { EXCLAMATION_MARK }
	| "=" { ASSIGN }
	| "+" { PLUS }
	| "-" { MINUS }
	| "*" { MULT }
	| "/" { DIV }
	| "%" { REM }
	| "&&" { AND }
	| "||" { OR }
	| "==" { EQUAL }
	| "!=" { NOTEQUAL }
	| "<=" { LANGLEEQUAL }
	| ">=" { RANGLEEQUAL }

(* keywords *)
	| "fn" { FN }
	| "let" { LET }
	| "if" { IF }
	| "else" { ELSE }
	| "return" { RETURN }
	| "true" { TRUE }
	| "false" { FALSE }

(* dynamic tokens *)
	| whitespace { read_token lexbuf }
	| int        { INT(int_of_string (Lexing.lexeme lexbuf)) }
	| float      { FLOAT(float_of_string (Lexing.lexeme lexbuf))}
	| id         { ID(Lexing.lexeme lexbuf) }
    | '"'        { read_string (Buffer.create 17) lexbuf }
	| newline    { next_line lexbuf; read_token lexbuf }

(* special tokens *)
	| eof        { EOF }
	| _          { raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
            read_string buf lexbuf
        }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }