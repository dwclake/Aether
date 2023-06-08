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
        Lexer.create(~input=code),
        1,
        tests
    )
};


let test_ident_tokens = () => {
    let code = "
        let five = 5;
        let ten = 10;

        let result = add(five, ten);
    ";

    let tests = [
        Token.LET,
        Token.IDENT("five"),
        Token.ASSIGN,
        Token.INT("5"),
        Token.SEMICOLON,

        Token.LET,
        Token.IDENT("ten"),
        Token.ASSIGN,
        Token.INT("10"),
        Token.SEMICOLON,

        Token.LET,
        Token.IDENT("result"),
        Token.ASSIGN,
        Token.IDENT("add"),
        Token.L_PAREN,
        Token.IDENT("five"),
        Token.COMMA,
        Token.IDENT("ten"),
        Token.R_PAREN,
        Token.SEMICOLON,
        Token.EOF
    ];

    test_token_seq(
        Lexer.create(~input=code),
        1,
        tests
    )
};

let test_operators = () => {
    let code = {|
        {};
        
        []$!-/*5\~`?'"%@^#;

        5 < 10 > 5;
    |};

    let tests = [
        Token.L_SQUIRELY,
        Token.R_SQUIRELY,
        Token.SEMICOLON,

        Token.L_BRACKET,
        Token.R_BRACKET,
        Token.DOLLAR,
        Token.BANG,
        Token.MINUS,
        Token.FORWARD_SLASH,
        Token.ASTERISK,
        Token.INT("5"),
        Token.BACK_SLASH,
        Token.TILDE,
        Token.BACK_TICK,
        Token.QUESTION,
        Token.SINGLE_QUOTE,
        Token.DOUBLE_QUOTE,
        Token.MODULO,
        Token.AT,
        Token.CARET,
        Token.POUND,
        Token.SEMICOLON,

        Token.INT("5"),
        Token.LESSER,
        Token.INT("10"),
        Token.GREATER,
        Token.INT("5"),
        Token.SEMICOLON,
        Token.EOF
    ];

    test_token_seq(
        Lexer.create(~input=code),
        1,
        tests
    )
};


let test_comp_ops = () => {
    let code = "
        let x = 12;
        let y = 1 =>;
        5 <= 10;
        x >= 2;
        y != x;
        y == 1;
    ";

    let tests = [
        Token.LET,
        Token.IDENT("x"),
        Token.ASSIGN,
        Token.INT("12"),
        Token.SEMICOLON,

        Token.LET,
        Token.IDENT("y"),
        Token.ASSIGN,
        Token.INT("1"),
        Token.FAT_ARROW,
        Token.SEMICOLON,

        Token.INT("5"),
        Token.LESSER_EQ,
        Token.INT("10"),
        Token.SEMICOLON,

        Token.IDENT("x"),
        Token.GREATER_EQ,
        Token.INT("2"),
        Token.SEMICOLON,

        Token.IDENT("y"),
        Token.NOT_EQUALS,
        Token.IDENT("x"),
        Token.SEMICOLON,

        Token.IDENT("y"),
        Token.EQUALS,
        Token.INT("1"),
        Token.SEMICOLON,
        Token.EOF
    ];

    test_token_seq(
        Lexer.create(~input=code),
        1,
        tests
    )
};

let test_keywords = () => {
    let code = "
        let y = true;
        bind x = match y {
            | true => 20
            | false => 10
        };

        if x > 5 {
            return 2;
        } else if x < 5 {
            1
        } else {
            0
        }
    ";

    let tests = [
        Token.LET,
        Token.IDENT("y"),
        Token.ASSIGN,
        Token.TRUE,
        Token.SEMICOLON,

        Token.BIND,
        Token.IDENT("x"),
        Token.ASSIGN,
        Token.MATCH,
        Token.IDENT("y"),
        Token.L_SQUIRELY,

        Token.PIPE,
        Token.TRUE,
        Token.FAT_ARROW,
        Token.INT("20"),

        Token.PIPE,
        Token.FALSE,
        Token.FAT_ARROW,
        Token.INT("10"),

        Token.R_SQUIRELY,
        Token.SEMICOLON,

        Token.IF,
        Token.IDENT("x"),
        Token.GREATER,
        Token.INT("5"),
        Token.L_SQUIRELY,

        Token.RETURN,
        Token.INT("2"),
        Token.SEMICOLON,

        Token.R_SQUIRELY,
        Token.ELSE,
        Token.IF,
        Token.IDENT("x"),
        Token.LESSER,
        Token.INT("5"),
        Token.L_SQUIRELY,

        Token.INT("1"),

        Token.R_SQUIRELY,
        Token.ELSE,
        Token.L_SQUIRELY,

        Token.INT("0"),
        
        Token.R_SQUIRELY,

        Token.EOF
    ];

    test_token_seq(
        Lexer.create(~input=code),
        1,
        tests
    )
};

let test_functions = () => {
    let code = "
        fn add (x, y) -> int {
            x + y
        };
    ";

    let tests = [
        Token.FN,
        Token.IDENT("add"),
        Token.L_PAREN,
        Token.IDENT("x"),
        Token.COMMA,
        Token.IDENT("y"),
        Token.R_PAREN,
        Token.SLIM_ARROW,
        Token.IDENT("int"),
        Token.L_SQUIRELY,

        Token.IDENT("x"),
        Token.PLUS,
        Token.IDENT("y"),

        Token.R_SQUIRELY,
        Token.SEMICOLON,

        Token.EOF
    ];

    test_token_seq(
        Lexer.create(~input=code),
        1,
        tests
    )
};