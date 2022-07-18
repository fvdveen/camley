open Lexer
open Lexing
open Interpreter

let print_error_position lexbuf = 
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf = 
    try Ok(Parser.program Lexer.read_token lexbuf) with
    | SyntaxError msg -> 
        let error_msg = Printf.sprintf "%s. %s@." (print_error_position lexbuf) msg in
        Error (error_msg)
    | Parser.Error ->
        let error_msg = Printf.sprintf "%s. syntax error@." (print_error_position lexbuf) in
        Error (error_msg)

let parse_program_from_string source =
    parse_program (Lexing.from_string source)

let run_program = eval_program