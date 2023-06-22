open Aether;
open Alcotest;

let tt = testable(Parser.pp_option_t, Parser.equal_option_t);
let ts = testable(Ast.pp_statement, Ast.equal_statement);

let check_parser_errors(li: list(string)) = {
    open List;
    open Stdio;

    if (length(li) == 0) {
        ()
    } else {
        eprintf("Parser had %d errors", length(li));
        let rec print = { fun
            | [] => {
                eprintf("\n"); 
                flush_all();
            }
            | [h,...t] => {
                eprintf("\n- parser error: %s", h);
                print(t)
            }
        };
        print(li)
    }
};

let rec test_token_seq(parser: Parser.t, ~i=1) = { fun
    | [] => ()
    | [h,...t] => {
        check(tt, string_of_int(i), h, parser.current);
        test_token_seq(Parser.next_token(parser), ~i=i + 1, t)
    }
};

let rec test_binding_statement_seq(~i=1, lists: (list(Ast.statement), list(Ast.statement))) = {
    switch lists {
        | ([], []) => ()
        | ([es,...et], [s,...t]) => {
            check(ts, string_of_int(i), es, s);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(s)));

            test_binding_statement_seq(~i=i + 1, (et, t))
        }
        | _ => failwith("Lists must be of the same size")
    }
};

let rec test_return_statement_seq(~i=1, lists: (list(Ast.statement), list(Ast.statement))) = {
    switch lists {
        | ([], []) => ()
        | ([es,...et], [s,...t]) => {
            check(ts, string_of_int(i), es, s);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(s)));

            test_return_statement_seq(~i=i + 1, (et, t))
        }
        | _ => failwith("Lists must be of the same size")
    }
};

let rec test_expression_statement_seq(~i=1, lists: (list(Ast.statement), list(Ast.statement))) = {
    switch lists {
        | ([], []) => ()
        | ([es,...et], [s,...t]) => {
            check(ts, string_of_int(i), es, s);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(s)));

            test_expression_statement_seq(~i=i + 1, (et, t))
        }
        | _ => failwith("Lists must be of the same size")
    }
};

let test_next_token(): unit = {
    let input = "=+(){},;";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    [   Some(Token.Assign),
        Some(Token.Plus),
        Some(Token.Lparen),
        Some(Token.Rparen),
        Some(Token.Lsquirly),
        Some(Token.Rsquirly),
        Some(Token.Comma),
        Some(Token.Semicolon),
        Some(Token.Eof)
    ] |> test_token_seq(parser)
};

let test_binding_statement(): unit = {
    let input = "
        let x = 5;
        const y = 10;
        let foobar = 838383;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    let test_length = 3;
    if (List.length(program.statements) != test_length) {
        failf("statements length is not %d, got=%d", test_length, List.length(program.statements))
    };

    ([  Ast.Let{name: "x", value: Ast.Integer(5)},
        Ast.Const{name: "y", value: Ast.Integer(10)},
        Ast.Let{name: "foobar", value: Ast.Integer(838383)}
     ], 
        program.statements
    ) 
    |> test_binding_statement_seq
}

let test_return_statement(): unit = {
    let input = "
        return 5;
        return 10;
        return 993322;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    let test_length = 3;
    if (List.length(program.statements) != test_length) {
        failf("statements length is not %d, got=%d", test_length, List.length(program.statements))
    };

    ([  Ast.Return{value: Ast.Integer(5)},
        Ast.Return{value: Ast.Integer(10)},
        Ast.Return{value: Ast.Integer(993322)}
     ], 
        program.statements
    ) 
    |> test_return_statement_seq
};

let test_identifier_expression(): unit = {
    let input = "
        foobar;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    let test_length = 1;
    if (List.length(program.statements) != test_length) {
        failf("statements length is not %d, got=%d", test_length, List.length(program.statements))
    };

    ([  Ast.ExpressionStatement{value: Ast.Identifier("foobar")}
     ], 
        program.statements
    ) 
    |> test_expression_statement_seq
};

let test_integer_expression(): unit = {
    let input = "
        5;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    let test_length = 1;
    if (List.length(program.statements) != test_length) {
        failf("statements length is not %d, got=%d", test_length, List.length(program.statements))
    };

    ([  Ast.ExpressionStatement{value: Ast.Integer(5)}
     ], 
        program.statements
    ) 
    |> test_expression_statement_seq
};

let test_float_expression(): unit = {
    let input = "
        5.4;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    let test_length = 1;
    if (List.length(program.statements) != test_length) {
        failf("statements length is not %d, got=%d", test_length, List.length(program.statements))
    };

    ([  Ast.ExpressionStatement{value: Ast.Float(5.4)}
     ], 
        program.statements
    ) 
    |> test_expression_statement_seq
};

let test_prefix_expression(): unit = {
    let input = "
        !5;
        -15;
        !foobar;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    let test_length = 3;
    if (List.length(program.statements) != test_length) {
        failf("statements length is not %d, got=%d", test_length, List.length(program.statements))
    };

    ([  Ast.ExpressionStatement{value: Ast.Prefix{operator: Token.Bang, value: Ast.Integer(5)}},
        Ast.ExpressionStatement{value: Ast.Prefix{operator: Token.Minus, value: Ast.Integer(15)}},
        Ast.ExpressionStatement{value: Ast.Prefix{operator: Token.Bang, value: Ast.Identifier("foobar")}},
     ], 
        program.statements
    ) 
    |> test_expression_statement_seq
};

let test_infix_expression(): unit = {
    let input = "
        5 + foobar;
        bar / 12;
        12.2 * 13;
        15 >= 13;
        a + b / c;
        -5 * !x;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    let test_length = 6;
    if (List.length(program.statements) != test_length) {
        failf("statements length is not %d, got=%d", test_length, List.length(program.statements))
    };

    ([  Ast.ExpressionStatement{value: Ast.Infix{
                lhs: Ast.Integer(5),
                operator: Token.Plus, 
                rhs: Ast.Identifier("foobar")
            }
        },
        Ast.ExpressionStatement{value: Ast.Infix{
                lhs: Ast.Identifier("bar"),
                operator: Token.Forwardslash, 
                rhs: Ast.Integer(12)
            }
        },
        Ast.ExpressionStatement{value: Ast.Infix{
                lhs: Ast.Float(12.2),
                operator: Token.Asterisk, 
                rhs: Ast.Integer(13)
            }
        },
        Ast.ExpressionStatement{value: Ast.Infix{
                lhs: Ast.Integer(15),
                operator: Token.GreaterEq, 
                rhs: Ast.Integer(13)
            }
        },
        Ast.ExpressionStatement{value: Ast.Infix{
                lhs: Ast.Identifier("a"),
                operator: Token.Plus, 
                rhs: Ast.Infix{
                    lhs: Ast.Identifier("b"),
                    operator: Token.Forwardslash,
                    rhs: Ast.Identifier("c")
                }
            }
        },
        Ast.ExpressionStatement{value: Ast.Infix{
                lhs: Ast.Prefix{
                    operator: Token.Minus,
                    value: Ast.Integer(5)
                },
                operator: Token.Asterisk, 
                rhs: Ast.Prefix{
                    operator: Token.Bang,
                    value: Ast.Identifier("x")
                }
            }
        },
     ], 
        program.statements
    ) 
    |> test_expression_statement_seq
};
