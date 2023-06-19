open Aether;
open Alcotest;

let tt = testable(Parser.pp_option_t, Parser.equal_option_t);
let ts = testable(Ast.pp_statement, Ast.equal_statement);
let ti = testable(Ast.pp_identifier, Ast.equal_identifier);

let check_parser_errors(l: list(string)) = {
    open List;
    open Stdio;

    if (length(l) == 0) {
        ()
    } else {
        eprintf("Parser had %d errors", length(l));
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
        print(l)
    }
};

let rec test_token_seq(p: Parser.t, ~i=1) = { fun
    | [] => ()
    | [et,...tl] => {
        check(tt, string_of_int(i), et, p.current);
        test_token_seq(Parser.next_token(p), ~i=i + 1, tl)
    }
};

let rec test_let_statement_seq(~i=1, l:(list(Ast.identifier), list(Ast.statement))) = {
    switch l {
        | ([], []) => ()
        | ([eid,...etl], [stmt,...tl]) => {
            let estmt = Ast.Let{name: eid, value: Identifier(eid)};

            let (ename, sname) = switch (estmt, stmt) {
                | (Let(e), Let(s)) => (e.name, s.name)
                | _ => failwith("Statement should be a let statement")
            };

            check(ti, string_of_int(i), ename, sname);
            check(ts, string_of_int(i), estmt, stmt);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(Ast.Statement(stmt)));

            test_let_statement_seq(~i=i + 1, (etl, tl))
        }
        | _ => failwith("Lists must be of the same size")
    }
};

let test_next_token() = {
    let input = "=+(){},;";

    let l = Lexer.create(~input);
    let p = Parser.create(l);

    [   Some(Token.Assign),
        Some(Token.Plus),
        Some(Token.Lparen),
        Some(Token.Rparen),
        Some(Token.Lsquirly),
        Some(Token.Rsquirly),
        Some(Token.Comma),
        Some(Token.Semicolon),
        Some(Token.Eof)
    ] |> test_token_seq(p)
};

let test_let_statement() = {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let l = Lexer.create(~input);
    let p = Parser.create(l);

    let (_, program) = Parser.parse_program(p);
    check_parser_errors(program.errors);

    if (List.length(program.statements) != 3) {
        failf("statements length is incorrect, got=%d",
            List.length(program.statements))
    };

    ([  {identifier:"x"},
        {identifier:"y"},
        {identifier:"foobar"}
     ], 
        program.statements
    ) 
    |> test_let_statement_seq
}

let test_return_statement() = {
    let input = "
        return 5;
        return 10;
        return 993322;
    ";

    let l = Lexer.create(~input);
    let p = Parser.create(l);

    let (_, program) = Parser.parse_program(p);
    check_parser_errors(program.errors);

    if (List.length(program.statements) != 3) {
        failf("statements length is incorrect, got=%d",
            List.length(program.statements))
    };

    let rec loop = { fun 
        | [] => ()
        | [s,...t] => {
            switch s {
                | Ast.Return(_) => loop(t)
                | _ as stmt => failf("Not a return statement, got %s",
                    Ast.show_statement(stmt)
                )
            }
        }
    };

    loop(program.statements)
};

let test_identifier_expression() = {
    let input = "
        foobar;
    ";

    let l = Lexer.create(~input);
    let p = Parser.create(l);

    let (_, program) = Parser.parse_program(p);
    check_parser_errors(program.errors);


    if (List.length(program.statements) != 1) {
        failf("statements length is incorrect, got=%d",
            List.length(program.statements))
    };

    let stmt = Core.List.nth(program.statements, 0);

    let ident = switch stmt {
        | Some(ExpressionStatement(e)) => {
            switch (e.value) {
                | Identifier(i) => Some(i)
            }
        }
        | _ => None
    };
    
    switch ident {
        | Some(i) => {
            if (i.identifier != "foobar") {
                failf("ident value not %s. got=%s",
                      "foobar",
                      i.identifier)
            }
        }
        | _ => failwith("Missing identifier")
    }
};
