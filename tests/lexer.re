open Lib
open Alcotest;

let tt = testable(Token.pp, (a, b) => a == b);

let rec test_token_seq = (l: Lexer.t, i: int, tests: list(Token.t)): unit => {
    let etok = List.nth(tests, i);
    let (l, tok) = Lexer.next_token(l);
    
    check(tt, string_of_int(i), etok, tok);

    if(tok != Token.EOF) { // Recursive case
        test_token_seq(l, i + 1, tests);
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


let suite = [
    (
        "Lexer",
        [
            test_case("Next token", `Quick, test_next_token)
        ]
    )
];

let () = run("Interpreter", suite);
