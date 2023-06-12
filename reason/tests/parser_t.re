open Lib
open Alcotest;

let tt = testable(Token.pp, (a, b) => a == b);

let ti = testable(Ast.pp_identifier, Ast.equal_identifier);

let rec test_token_seq = (p: Parser.t, i: int) => { fun
    | [] => ()
    | [etok, ...tail] => {
        check(tt, string_of_int(i), etok, p.cur_t);
        test_token_seq(Parser.next_token(p), i + 1, tail);
    }
};
let rec test_statement_seq = (i: int, l:(list(Ast.identifier), list(Ast.statement))) => {
    switch l {
        | ([], []) => ()
        | ([eid, ...etl], [st, ...tl]) => {
            let ident = switch st {
                | LET(s) => s.name
            };

            check(ti, string_of_int(i), eid, ident);
            test_statement_seq(i + 1, (etl, tl));
        }
        | _ => ()
    }
};

let test_next_token = () => {
    let code = "=+(){},;";

    let tests = [
        Token.ASSIGN,
        Token.PLUS,
        Token.LPAREN,
        Token.RPAREN,
        Token.LSQUIRLY,
        Token.RSQUIRLY,
        Token.COMMA,
        Token.SEMICOLON,
        Token.EOF
    ];

    test_token_seq(
        Parser.create(
            Lexer.create(~input=code)
        ),
        1,
        tests
    )
};

let test_let_statement = () => {
    let code = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    ";

    let p = Parser.create(Lexer.create(~input=code));

    let program = switch (Parser.parse_program(p)) {
        | Ok(prog) => prog
        | Error(e) => failwith(e) 
    };
    if(List.length(program.statements) != 3) {
        failwith("Program statements list length is incorrect")
    }

    let tests: list(Ast.identifier) = [
        {identifier:"x"},
        {identifier:"y"},
        {identifier:"foobar"}
    ];

    test_statement_seq(
        1,
        (tests, program.statements),
    )
}
