open Aether;
open Alcotest;

let tt = testable(Parser.pp_option_t, Parser.equal_option_t);
let ts = testable(Ast.pp_statement, Ast.equal_statement);
let ti = testable(Ast.pp_identifier, Ast.equal_identifier);

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
    | [etok,...tail] => {
        check(tt, string_of_int(i), etok, parser.current);
        test_token_seq(Parser.next_token(parser), ~i=i + 1, tail)
    }
};

let rec test_let_statement_seq(~i=1, lists:(list(Ast.identifier), list(Ast.statement))) = {
    switch lists {
        | ([], []) => ()
        | ([eident,...etail], [stmt,...tail]) => {
            let estmt = Ast.Let{name: eident, value: Identifier(eident)};

            let (ename, sname) = switch (estmt, stmt) {
                | (Let(e), Let(s)) => (e.name, s.name)
                | _ => failwith("Statement should be a let statement")
            };

            check(ti, string_of_int(i), ename, sname);
            check(ts, string_of_int(i), estmt, stmt);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(stmt)));

            test_let_statement_seq(~i=i + 1, (etail, tail))
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

    ([  {identifier:"x"},
        {identifier:"y"},
        {identifier:"foobar"}
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

    let rec loop = { fun 
        | [] => ()
        | [s,...t] => {
            switch s {
                | Ast.Return(_) => loop(t)
                | _ as stmt => failf("Not a return statement, got %s", Ast.show_statement(stmt))
            }
        }
    };

    loop(program.statements)
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
        | Some(ExpressionStatement(expr)) => {
            switch (expr.value) {
                | Identifier(i) => Some(i)
            }
        }
        | _ => None
    };
    
    switch ident {
        | Some(i) => {
            check(Alcotest.string, "1", "foobar", i.identifier)
        }
        | _ => failwith("Missing identifier")
    }
};
