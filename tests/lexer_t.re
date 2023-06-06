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


let test_ident_tokens = () => {
    let code = {|
        let five = 5;
        bind ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
    |};

    let tests = [
        (Token.LET),
        (Token.IDENT("five")),
        (Token.ASSIGN),
        (Token.INT("5")),
        (Token.SEMICOLON),
        (Token.BIND),
        (Token.IDENT("ten")),
        (Token.ASSIGN),
        (Token.INT("10")),
        (Token.SEMICOLON),
        (Token.LET),
        (Token.IDENT("add")),
        (Token.ASSIGN),
        (Token.FN),
        (Token.L_PAREN),
        (Token.IDENT("x")),
        (Token.COMMA),
        (Token.IDENT("y")),
        (Token.R_PAREN),
        (Token.L_SQUIRELY),
        (Token.IDENT("x")),
        (Token.PLUS),
        (Token.IDENT("y")),
        (Token.SEMICOLON),
        (Token.R_SQUIRELY),
        (Token.SEMICOLON),
        (Token.LET),
        (Token.IDENT("result")),
        (Token.ASSIGN),
        (Token.IDENT("add")),
        (Token.L_PAREN),
        (Token.IDENT("five")),
        (Token.COMMA),
        (Token.IDENT("ten")),
        (Token.R_PAREN),
        (Token.SEMICOLON),
        (Token.EOF)
    ];

    test_token_seq(
        Lexer.create(~input=code),
        0,
        tests
    )
}
