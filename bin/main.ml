open Camley.Interface
open Camley.Object
open Camley.Pretty_print

let () = 
    match (parse_program_from_string  "let loop_range = fn(loop_range, init, cond, iter, body) {
    if (cond(init)) {
        body(init);
        loop_range(loop_range, iter(init), cond, iter, body);
    };
};
loop_range(
    loop_range,
    0, 
    fn(x) { x < 10; },
    fn(x) { x + 1; },
    fn(x) { x + 2; }
);
") with
    | Error err -> Printf.printf "ERROR: %s\n" err
    | Ok prog -> pprint_program prog;
        match run_program prog with
        | Error err -> Printf.printf "ERROR: %s\n" err
        | Ok(_, value) -> Printf.printf "%s\n" (string_of_object value)
