open Lib
open Alcotest;

let tt = testable(Token.pp, (a, b) => a == b);

let rec test_token_seq = (l: Lexer.t, i: int) => { fun
    | [] => ()
    | [etok, ...tail] => {
        let (l, tok) = Lexer.next_token(l);

        check(tt, string_of_int(i), etok, tok);
        test_token_seq(l, i + 1, tail);
    }
}

let test_next_token = () => {
    let code = {|=+(){},;|};

    let tests = [
        (Token.ASSIGN),
        (Token.PLUS),
        (Token.L_PAREN),
        (Token.R_PAREN),
        (Token.L_SQUIRELY),
        (Token.R_SQUIRELY),
        (Token.COMMA),
        (Token.SEMICOLON),
        (Token.EOF)
    ];

    test_token_seq(
        Lexer.create(~input=code),
        0,
        tests
    )
}
