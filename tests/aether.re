open Alcotest;
open Lexer_t;

let suite = [
    (
        "Lexer",
        [
            test_case("Next token", `Quick, test_next_token),
            test_case("Identifiers, Ints", `Quick, test_ident_tokens),
            test_case("Operators", `Quick, test_operators),
            test_case("Compound operators", `Quick, test_comp_ops),
            test_case("Keywords", `Quick, test_keywords),
            test_case("Functions", `Quick, test_functions)
        ]
    ),
];

let () = run("Interpreter", suite);