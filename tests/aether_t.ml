open Alcotest

let suite = 
    [ ("Lexer",
        [ test_case "Next token" `Quick Lexer_t.test_next_token
        ; test_case "Identifiers, Ints" `Quick Lexer_t.test_ident_tokens
        ; test_case "Operators" `Quick Lexer_t.test_operators
        ; test_case "Compound operators" `Quick Lexer_t.test_comp_ops
        ; test_case "Keywords" `Quick Lexer_t.test_keywords
        ; test_case "Functions" `Quick Lexer_t.test_functions
        ])
    ; ("Parser",
        [ test_case "Binding statement" `Quick Parser_t.test_binding_statement
        ; test_case "Return statement" `Quick Parser_t.test_return_statement
        ; test_case "Identifier expression statement" `Quick Parser_t.test_identifier_expression
        ; test_case "Integer expression" `Quick Parser_t.test_integer_expression
        ; test_case "Float expression" `Quick Parser_t.test_float_expression
        ; test_case "Boolean expression" `Quick Parser_t.test_boolean_expression
        ; test_case "If expression" `Quick Parser_t.test_if_expression
        ; test_case "If else expression" `Quick Parser_t.test_if_else_expression
        ; test_case "Fn literal expression" `Quick Parser_t.test_fn_literal_expression
        ; test_case "Prefix expression" `Quick Parser_t.test_prefix_expression
        ; test_case "Infix expression" `Quick Parser_t.test_infix_expression
        ; test_case "Complex expression" `Quick Parser_t.test_complex_parsing
        ])
    ; ("Ast",
        [test_case "Test string" `Quick Ast_t.test_string;]
        )
    ]

let () = 
    run "Interpreter" suite
;;
