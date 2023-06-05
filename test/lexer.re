open Lib

let code = "
=+(){},;
";

let tests = [
    (Token.ASSIGN, "="),
    (Token.PLUS, "+"),
    (Token.L_PAREN, "("),
    (Token.R_PAREN, ")"),
    (Token.L_SQUIRELY, "{"),
    (Token.R_SQUIRELY, "}"),
    (Token.COMMA, ","),
    (Token.SEMICOLON, ";"),
    (Token.EOF, "")
];

let l = Lexer.new();

let run_test = (i: int, test: (Token.t, string)): unit => {
    let (t, s) = test;
    tok = Lexer.next_token(l);

    if(Token.from_string(tok) != t) {
        Stdio.printf("Test #%d - token type wrong. expected %s, got %s", i, s, tok);
    }

    if(tok != s) {
        Stdio.printf("Test #%d - literal wrong. expected %s, got %s", i, s, tok);
    } 
}

let _ = List.iteri
