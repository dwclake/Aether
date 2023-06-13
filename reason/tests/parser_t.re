open Lib
open Alcotest;

let tt = testable(Token.pp, (a, b) => a == b);
let ti = testable(Ast.pp_identifier, Ast.equal_identifier);

let rec test_token_seq = (p: Parser.t, ~i= 1) => { fun
    | [] => ()
    | [et,...tl] => {
        check(tt, string_of_int(i), et, p.cur_t);
        test_token_seq(Parser.next_token(p), ~i=i + 1, tl);
    }
};

let rec test_statement_seq = (~i= 1, l:(list(Ast.identifier), list(Ast.statement))) => {
    switch l {
        | ([], []) => ()
        | ([ei,...etl], [st,...tl]) => {
            let ident = switch st {
                | LET(s) => s.name
            };

            check(ti, string_of_int(i), ei, ident);
            test_statement_seq(~i=i + 1, (etl, tl));
        }
        | _ => ()
    }
};

let test_next_token = () => {
    let code = "=+(){},;";
    let p = Parser.create(Lexer.create(~input=code));

    [
        Token.ASSIGN,
        Token.PLUS,
        Token.LPAREN,
        Token.RPAREN,
        Token.LSQUIRLY,
        Token.RSQUIRLY,
        Token.COMMA,
        Token.SEMICOLON,
        Token.EOF
    ] |> test_token_seq(p)
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
    };

    ([  {identifier:"x"},
        {identifier:"y"},
        {identifier:"foobar"}
     ], 
        program.statements
    ) |> test_statement_seq
}
