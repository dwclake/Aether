open Lib
open Alcotest;

let tt = testable(Token.pp, (a, b) => a == b);

let rec test_token_seq = (p: Parser.t, i: int) => { fun
    | [] => ()
    | [etok, ...tail] => {
        check(tt, string_of_int(i), etok, p.cur_t);
        test_token_seq(Parser.next_token(p), i + 1, tail);
    }
};

let test_next_token = () => {
    let code = "=+(){},;";

    let tests = [
        Token.ASSIGN,
        Token.PLUS,
        Token.L_PAREN,
        Token.R_PAREN,
        Token.L_SQUIRELY,
        Token.R_SQUIRELY,
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
