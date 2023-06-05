open Alcotest;
open Lexer_t;

let suite = [
    (
        "Lexer",
        [
            test_case("Next token", `Quick, test_next_token)
        ]
    ),
];

let () = run("Interpreter", suite);
