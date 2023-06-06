open Alcotest;
open Lexer_t;

let suite = [
    (
        "Lexer",
        [
            test_case("Next token", `Quick, test_next_token),
            test_case("Idents, some keywords, ints", `Quick, test_ident_tokens)
        ]
    ),
];

let () = run("Interpreter", suite);
