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

let rec test_statement_seq(~i=1, lists: (list(Ast.statement), list(Ast.statement))) = {
    switch lists {
        | ([], []) => ()
        | ([es,...et], [s,...t]) => {
            check(ts, string_of_int(i), es, s);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(s)));

            test_statement_seq(~i=i + 1, (et, t))
        }
        | _ => failwith("Lists must be of the same size")
    }
};

let check_stmts_len(program: Ast.program, len) = {
    if (List.length(program.statements) != len) {
        failf("statements length is not %d, got=%d", len, List.length(program.statements))
    }
}

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
    let num_tests = 3;
    let input = "
        let x = 5;
        const y = 10;
        let foobar = 838383;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Binding{kind: Token.Let, name: "x", value: Ast.Integer(5)},
        Ast.Binding{kind: Token.Const, name: "y", value: Ast.Integer(10)},
        Ast.Binding{kind: Token.Let, name: "foobar", value: Ast.Integer(838383)}
     ], 
        program.statements
    ) 
    |> test_statement_seq
}

let test_return_statement(): unit = {
    let num_tests = 3;
    let input = "
        return 5;
        return 10;
        return 993322;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Return{value: Ast.Integer(5)},
        Ast.Return{value: Ast.Integer(10)},
        Ast.Return{value: Ast.Integer(993322)}
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_identifier_expression(): unit = {
    let num_tests = 1;
    let input = "
        foobar;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.Identifier("foobar")}
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_integer_expression(): unit = {
    let num_tests = 1;
    let input = "
        5;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.Integer(5)}
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_float_expression(): unit = {
    let num_tests = 1;
    let input = "
        5.4;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.Float(5.4)}
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_boolean_expression(): unit = {
    let num_tests = 4;
    let input = "
        true;
        !false;
        const foobar = true;
        let barfoo = false;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.Boolean(true)},
        Ast.Expression{value: Ast.Prefix{operator: Token.Bang, value: Ast.Boolean(false)}},
        Ast.Binding{kind: Token.Const, name: "foobar", value: Ast.Boolean(true)},
        Ast.Binding{kind: Token.Let, name: "barfoo", value: Ast.Boolean(false)},
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_prefix_expression(): unit = {
    let num_tests = 3;
    let input = "
        !5;
        -15;
        !foobar;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.Prefix{operator: Token.Bang, value: Ast.Integer(5)}},
        Ast.Expression{value: Ast.Prefix{operator: Token.Minus, value: Ast.Integer(15)}},
        Ast.Expression{value: Ast.Prefix{operator: Token.Bang, value: Ast.Identifier("foobar")}},
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_infix_expression(): unit = {
    let num_tests = 8;
    let input = "
        5 + foobar;
        bar / 12;
        12.2 * 13;
        15 >= 13;
        a + b / c;
        -5 * !x;
        3 > 5 == false;
        1 + (2 + 3) + 4;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.Infix{
                lhs: Ast.Integer(5),
                operator: Token.Plus, 
                rhs: Ast.Identifier("foobar")
            }
        },
        Ast.Expression{value: Ast.Infix{
                lhs: Ast.Identifier("bar"),
                operator: Token.Forwardslash, 
                rhs: Ast.Integer(12)
            }
        },
        Ast.Expression{value: Ast.Infix{
                lhs: Ast.Float(12.2),
                operator: Token.Asterisk, 
                rhs: Ast.Integer(13)
            }
        },
        Ast.Expression{value: Ast.Infix{
                lhs: Ast.Integer(15),
                operator: Token.GreaterEq, 
                rhs: Ast.Integer(13)
            }
        },
        Ast.Expression{value: Ast.Infix{
                lhs: Ast.Identifier("a"),
                operator: Token.Plus, 
                rhs: Ast.Infix{
                    lhs: Ast.Identifier("b"),
                    operator: Token.Forwardslash,
                    rhs: Ast.Identifier("c")
                }
            }
        },
        Ast.Expression{value: Ast.Infix{
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
        Ast.Expression{value: Ast.Infix{
                lhs: Ast.Infix{
                    lhs: Ast.Integer(3),
                    operator: Token.Greater,
                    rhs: Ast.Integer(5)
                },
                operator: Token.EqualTo, 
                rhs: Ast.Boolean(false)
            }
        },
        Ast.Expression{value: Ast.Infix{
                lhs: Ast.Infix{
                    lhs: Ast.Integer(1),
                    operator: Token.Plus,
                    rhs: Ast.Infix{
                        lhs: Ast.Integer(2),
                        operator: Token.Plus,
                        rhs: Ast.Integer(3)
                    }
                },
                operator: Token.Plus, 
                rhs: Ast.Integer(4)
            }
        },
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_if_expression(): unit = {
    let num_tests = 1;
    let input = "
        if x < y { x }
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.If{
            condition: Ast.Infix{
                lhs: Ast.Identifier("x"), 
                operator: Token.Lesser,
                rhs: Ast.Identifier("y")
            }, 
            consequence: [Ast.Expression{value: Ast.Identifier("x")}],
            alternative: None
        }},
     ], 
        program.statements
    ) 
    |> test_statement_seq
};

let test_if_else_expression(): unit = {
    let num_tests = 1;
    let input = "
        if (x < y) { 
            x 
        } else { 
            y;
        }
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);
    check_stmts_len(program, num_tests);

    ([  Ast.Expression{value: Ast.If{
            condition: Ast.Infix{
                lhs: Ast.Identifier("x"), 
                operator: Token.Lesser,
                rhs: Ast.Identifier("y")
            }, 
            consequence: [Ast.Expression{value: Ast.Identifier("x")}],
            alternative: Some([Ast.Expression{value: Ast.Identifier("y")}])
        }},
     ], 
        program.statements
    ) 
    |> test_statement_seq
};
