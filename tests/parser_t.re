open Aether;
open Alcotest;

let tt = testable(Parser.pp_option_t, Parser.equal_option_t);
let ts = testable(Ast.pp_statement, Ast.equal_statement);
let ti = testable(Ast.pp_identifier, Ast.equal_identifier);
let te = testable(Ast.pp_expression, Ast.equal_expression);

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

let rec test_let_statement_seq(~i=1, lists:(list(Ast.statement), list(Ast.statement))) = {
    switch lists {
        | ([], []) => ()
        | ([es,...et], [s,...t]) => {
            let (ename, name, evalue, value) = switch (es, s) {
                | (Let({name: ename, value: evalue}), 
                   Let({name, value})) => {
                    (ename, name, evalue, value)
                }
                | _ => failwith("Statement should be a let statement")
            };

            check(ti, string_of_int(i), ename, name);
            check(ts, string_of_int(i), es, s);
            check(te, string_of_int(i), evalue, value);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(s)));

            test_let_statement_seq(~i=i + 1, (et, t))
        }
        | _ => failwith("Lists must be of the same size")
    }
};

let rec test_return_statement_seq(~i=1, lists:(list(Ast.statement), list(Ast.statement))) = {
    switch lists {
        | ([], []) => ()
        | ([es,...et], [s,...t]) => {
            let (evalue, value) = switch (es, s) {
                | (Return(eexpr), Return(expr)) => (eexpr.value, expr.value)
                | _ => failwith("Statement should be a return statement")
            };

            check(ts, string_of_int(i), es, s);
            check(te, string_of_int(i), evalue, value);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(s)));

            test_return_statement_seq(~i=i + 1, (et, t))
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

let test_let_statement(): unit = {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);

    if (List.length(program.statements) != 3) {
        failf("statements length is incorrect, got=%d", List.length(program.statements))
    };

    ([  Ast.Let{name: {identifier: "x"}, value: Ast.Integer{value: "5"}},
        Ast.Let{name: {identifier: "y"}, value: Ast.Integer{value: "10"}},
        Ast.Let{name: {identifier: "foobar"}, value: Ast.Integer{value: "838383"}}
     ], 
        program.statements
    ) 
    |> test_let_statement_seq
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

    if (List.length(program.statements) != 3) {
        failf("statements length is incorrect, got=%d", List.length(program.statements))
    };

    ([  Ast.Return{value: Ast.Integer{value: "5"}},
        Ast.Return{value: Ast.Integer{value: "10"}},
        Ast.Return{value: Ast.Integer{value: "993322"}}
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


    if (List.length(program.statements) != 1) {
        failf("statements length is incorrect, got=%d", List.length(program.statements))
    };

    let stmt = Core.List.nth(program.statements, 0);

    let ident = switch stmt {
        | Some(ExpressionStatement{value}) => {
            switch (value) {
                | Identifier(ident) => Some(ident)
                | _ => None
            }
        }
        | _ => None
    };
    
    switch ident {
        | Some({identifier}) => {
            check(Alcotest.string, "1", "foobar", identifier)
        }
        | _ => failwith("Missing identifier")
    }
};

let test_integer_expression(): unit = {
    let input = "
        5;
    ";

    let lexer = Lexer.create(~input);
    let parser = Parser.create(lexer);

    let (_, program) = Parser.parse_program(parser);
    check_parser_errors(program.errors);


    if (List.length(program.statements) != 1) {
        failf("statements length is incorrect, got=%d", List.length(program.statements))
    };

    let stmt = Core.List.nth(program.statements, 0);

    let value = switch stmt {
        | Some(Ast.ExpressionStatement{value}) => {
            switch (value) {
                | Integer(value) => Some(value)
                | _ => None
            }
        }
        | _ => None
    };
    
    switch value {
        | Some({value}) => {
            check(Alcotest.string, "1", "5", value)
        }
        | _ => failwith("Missing value")
    }
};
