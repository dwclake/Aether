open Alcotest

let suite = [
    (   "Lexer",
        [   test_case "Next token" `Quick Lexer_t.test_next_token;
            test_case "Identifiers, Ints" `Quick Lexer_t.test_ident_tokens;
            test_case "Operators" `Quick Lexer_t.test_operators;
            test_case "Compound operators" `Quick Lexer_t.test_comp_ops;
            test_case "Keywords" `Quick Lexer_t.test_keywords;
            test_case "Functions" `Quick Lexer_t.test_functions;
        ]);
    (   "Parser",
        [   test_case "Next token" `Quick Parser_t.test_next_token;
            test_case "Let statement" `Quick Parser_t.test_let_statement;
            test_case "Return statement" `Quick Parser_t.test_return_statement;
        ]);
];;

let () = 
    run "Interpreter" suite
;;
