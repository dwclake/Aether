open Aether;
open Alcotest;

let tt = testable(Token.pp, Token.equal);
let ts = testable(Ast.pp_statement, Ast.equal_statement);
let ti = testable(Ast.pp_identifier, Ast.equal_identifier);

let check_parser_errors(p: Parser.t) = {
    open List;
    open Stdio;

    if (length(p.errors) == 0) {
        ()
    } else {
        eprintf("\nParser had %d errors", length(p.errors));
        let rec print = { fun
            | [] => eprintf("\n\nend of parser error list")
            | [h,...t] => {
                eprintf("\n- parser error: %s", h);
                print(t)
            }
        };
        print(p.errors)
    }
};

let rec test_token_seq(p: Parser.t, ~i= 1) = { fun
    | [] => ()
    | [et,...tl] => {
        check(tt, string_of_int(i), et, p.cur_t);
        test_token_seq(Parser.next_token(p), ~i=i + 1, tl)
    }
};

let rec test_let_statement_seq(~i=1, l:(list(Ast.identifier), list(Ast.node))) = {
    switch l {
        | ([], []) => ()
        | ([eid,...etl], [nd,...tl]) => {
            let stmt = switch nd {
                | Statement(s) => s
                | _ => failwith("Node should be a statement")
            };
            let estmt = Ast.Let{name: eid, value: Identifier(eid)};

            let (ename, sname) = switch (estmt, stmt) {
                | (Let(e), Let(s)) => (e.name, s.name)
                | _ => failwith("Statement should be a let statement")
            };

            check(ti, string_of_int(i), ename, sname);
            check(ts, string_of_int(i), estmt, stmt);
            check(Alcotest.string, string_of_int(i), "statement", Ast.token_literal(nd));
            test_let_statement_seq(~i=i + 1, (etl, tl))
        }
        | _ => failwith("Lists must be of the same size")
    }
};

let test_next_token() = {
    let code = "=+(){},;";
    let p = Parser.create(Lexer.create(~input=code));

    [   Token.Assign,
        Token.Plus,
        Token.Lparen,
        Token.Rparen,
        Token.Lsquirly,
        Token.Rsquirly,
        Token.Comma,
        Token.Semicolon,
        Token.Eof
    ] |> test_token_seq(p)
};

let test_let_statement() = {
    let code = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";
    let p = Parser.create(Lexer.create(~input=code));

    let (p, program) = Parser.parse_program(p);
    check_parser_errors(p);

    if (List.length(program.statements) != 3) {
        failwith("Program statements list length is incorrect")
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
    let code = "
        return 5;
        return 10;
        return 993322;
    ";

    let l = Lexer.create(~input=code);
    let p = Parser.create(l);

    let (p, program) = Parser.parse_program(p);
    check_parser_errors(p);

    if (List.length(program.statements) != 3) {
        failwith("Program statements list length is incorrect")
    };

    let rec loop = { fun 
        | [] => ()
        | [h,...t] => {
            switch h {
                | Ast.Statement(s) => {
                    switch s {
                        | Ast.Return(_) => loop(t)
                        | _ as ret => failwith(Format.sprintf(
                            "Not a return statement, got %s",
                            Ast.show_statement(ret)
                        ))
                    }
                }
                | _ as nd => failwith(Format.sprintf(
                    "Not a statement node, got %s",
                    Ast.show_node(nd)
                ))
            }
        }
    };

    loop(program.statements)
};
