open Aether;
open Alcotest;

let tt = testable(Token.pp, Token.equal);

let rec test_token_seq(lexer: Lexer.t, ~i= 1) = { fun
    | [] => ()
    | [h,...t] => {
        let lex = Lexer.next_token(lexer);

        check(tt, string_of_int(i), h, lex#token);
        test_token_seq(lex#lexer, ~i=i + 1, t);
    }
};

let test_next_token() = {
    let input = "=+(){},;";

    [   Token.Assign,
        Token.Plus,
        Token.Lparen,
        Token.Rparen,
        Token.Lsquirly,
        Token.Rsquirly,
        Token.Comma,
        Token.Semicolon,
        Token.Eof
    ] 
    |> test_token_seq(Lexer.create(~input))
};


let test_ident_tokens() = {
    let input = "
        let five = 5;
        let ten = 10.0;

        let result = add(five, ten);
    ";

    [   Token.Let,
        Token.Ident("five"),
        Token.Assign,
        Token.Int("5"),
        Token.Semicolon,

        Token.Let,
        Token.Ident("ten"),
        Token.Assign,
        Token.Float("10.0"),
        Token.Semicolon,

        Token.Let,
        Token.Ident("result"),
        Token.Assign,
        Token.Ident("add"),
        Token.Lparen,
        Token.Ident("five"),
        Token.Comma,
        Token.Ident("ten"),
        Token.Rparen,
        Token.Semicolon,
        Token.Eof
    ] 
    |> test_token_seq(Lexer.create(~input))
};

let test_operators() = {
    let input = {|
        {};
        
        []$!-/*5\~`?'"%@^#;

        5 < 10 > 5;
    |};

    [   Token.Lsquirly,
        Token.Rsquirly,
        Token.Semicolon,

        Token.Lbracket,
        Token.Rbracket,
        Token.Dollar,
        Token.Bang,
        Token.Minus,
        Token.Forwardslash,
        Token.Asterisk,
        Token.Int("5"),
        Token.Backslash,
        Token.Tilde,
        Token.Backtick,
        Token.Question,
        Token.SingleQuote,
        Token.DoubleQuote,
        Token.Modulo,
        Token.At,
        Token.Caret,
        Token.Pound,
        Token.Semicolon,

        Token.Int("5"),
        Token.Lesser,
        Token.Int("10"),
        Token.Greater,
        Token.Int("5"),
        Token.Semicolon,
        Token.Eof
    ] 
    |> test_token_seq(Lexer.create(~input))
};


let test_comp_ops() = {
    let input = "
        let x = 12;
        let y = 1 =>;
        5 <= 10;
        x >= 2;
        y != x;
        y == 1;
    ";

    [   Token.Let,
        Token.Ident("x"),
        Token.Assign,
        Token.Int("12"),
        Token.Semicolon,

        Token.Let,
        Token.Ident("y"),
        Token.Assign,
        Token.Int("1"),
        Token.FatArrow,
        Token.Semicolon,

        Token.Int("5"),
        Token.LesserEq,
        Token.Int("10"),
        Token.Semicolon,

        Token.Ident("x"),
        Token.GreaterEq,
        Token.Int("2"),
        Token.Semicolon,

        Token.Ident("y"),
        Token.NotEq,
        Token.Ident("x"),
        Token.Semicolon,

        Token.Ident("y"),
        Token.EqualTo,
        Token.Int("1"),
        Token.Semicolon,
        Token.Eof
    ] 
    |> test_token_seq(Lexer.create(~input))
};

let test_keywords() = {
    let input = "
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

    [   Token.Let,
        Token.Ident("y"),
        Token.Assign,
        Token.True,
        Token.Semicolon,

        Token.Bind,
        Token.Ident("x"),
        Token.Assign,
        Token.Match,
        Token.Ident("y"),
        Token.Lsquirly,

        Token.Pipe,
        Token.True,
        Token.FatArrow,
        Token.Int("20"),

        Token.Pipe,
        Token.False,
        Token.FatArrow,
        Token.Int("10"),

        Token.Rsquirly,
        Token.Semicolon,

        Token.If,
        Token.Ident("x"),
        Token.Greater,
        Token.Int("5"),
        Token.Lsquirly,

        Token.Return,
        Token.Int("2"),
        Token.Semicolon,

        Token.Rsquirly,
        Token.Else,
        Token.If,
        Token.Ident("x"),
        Token.Lesser,
        Token.Int("5"),
        Token.Lsquirly,

        Token.Int("1"),

        Token.Rsquirly,
        Token.Else,
        Token.Lsquirly,

        Token.Int("0"),
        
        Token.Rsquirly,

        Token.Eof
    ] 
    |> test_token_seq(Lexer.create(~input))
};

let test_functions() = {
    let input = "
        let add = fn(x, y) -> Int {
            x + y
        };
    ";

    [   Token.Let,
        Token.Ident("add"),
        Token.Assign,
        Token.Fn,
        Token.Lparen,
        Token.Ident("x"),
        Token.Comma,
        Token.Ident("y"),
        Token.Rparen,
        Token.SlimArrow,
        Token.Ident("Int"),
        Token.Lsquirly,

        Token.Ident("x"),
        Token.Plus,
        Token.Ident("y"),

        Token.Rsquirly,
        Token.Semicolon,

        Token.Eof
    ] 
    |> test_token_seq(Lexer.create(~input))
};
