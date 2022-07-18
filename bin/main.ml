open Camley.Interface
open Camley.Object

let () = 
    match (parse_program_from_string  "let x = { let y = 6; y;}; x; y;") with
    | Error err -> Printf.printf "ERROR: %s\n" err
    | Ok prog -> match run_program prog with
        | Error err -> Printf.printf "ERROR: %s\n" err
        | Ok(_, value) -> Printf.printf "%s\n" (string_of_object value)
