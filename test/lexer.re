open Lib

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

let l = Lexer.create(~input=code);

let rec run_test = (l: Lexer.t, i: int, tests: list(Token.t)): unit => {
    let etok = List.nth(tests, i);
    let (l, tok) = Lexer.next_token(l);

    if(tok != etok) {
        Stdio.printf("Test #%d - token type wrong. expected %s, got %s\n", i, Token.to_string(etok), Token.to_string(tok));
    } 

    if(tok != Token.EOF) { // Recursive case
        run_test(l, i + 1, tests);
    }
}

let () = {
    Stdio.printf("Running Lexer Test:\n");
    run_test(l, 0, tests);
}
