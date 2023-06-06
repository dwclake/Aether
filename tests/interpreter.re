open Alcotest;
open Lexer_t;

let suite = [
    (
        "Lexer",
        [
            test_case("Next token", `Quick, test_next_token),
            test_case("Complex tokens", `Quick, test_complex_tokens)
        ]
    ),
];

let () = run("Interpreter", suite);
