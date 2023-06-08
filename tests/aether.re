open Alcotest;
open Lexer_t;

let suite = [
    (
        "Lexer",
        [
            test_case("Next token", `Quick, test_next_token),
            test_case("Idents, some keywords, ints", `Quick, test_ident_tokens),
            test_case("More operators", `Quick, test_more_operators),
            test_case("Compound operators", `Quick, test_comp_ops),
            test_case("More keywords", `Quick, test_more_keywords)
        ]
    ),
];

let () = run("Interpreter", suite);
