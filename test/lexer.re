open Lib

let code = "=+(){},;";

let tests = [
    (Token.ASSIGN, '='),
    (Token.PLUS, '+'),
    (Token.L_PAREN, '('),
    (Token.R_PAREN, ')'),
    (Token.L_SQUIRELY, '{'),
    (Token.R_SQUIRELY, '}'),
    (Token.COMMA, ','),
    (Token.SEMICOLON, ';'),
    (Token.EOF, '\000')
];

let l = Lexer.create(~input=code);

let rec run_test = (l: Lexer.t, i: int, tests): unit => {

    let i = if(i >= List.length(tests)) {
        List.length(tests) - 1;
    } else {
        i
    }

    let (t, c) = List.nth(tests, i);
    let (l, tok) = Lexer.next_token(l);

    if(tok != t) {
        Stdio.printf("Test #%d - token type wrong. expected %s, got %s\n", i, Token.to_string(t), Token.to_string(tok));
    } 

    let tok_char = switch(Token.to_char(tok)) {
        | Some(ch) => ch
        | None => '\000'
    };
    if(tok_char != c) {
        Stdio.printf("Test #%d - literal wrong. expected %c, got %s\n\n", i, c, Token.to_string(tok));
    }

    if(tok != Token.EOF) {
        run_test(
        l,
        i + 1,
        tests
        )
    } else {
        ()
    }
}

let () = {
    Stdio.printf("Running Lexer Test:\n");
    run_test(l, 0, tests);
}
