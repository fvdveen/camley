open Camley.Interface
open Camley.Object

let () = 
    match (parse_program_from_string  "5;") with
    | Error err -> Printf.printf "ERROR: %s\n" err
    | Ok prog -> match run_program prog with
        | Error err -> Printf.printf "ERROR: %s\n" err
        | Ok(_, value) -> Printf.printf "%s\n" (object_to_string value)