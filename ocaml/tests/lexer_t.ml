open Aether
open Alcotest

let tt = testable Token.pp Token.equal

let rec test_token_seq (l: Lexer.t) ?(i = 1) = function
    | [] -> ()
    | (et::tl) -> (
        let lex = Lexer.next_token l in

        check tt (string_of_int i) et lex#tok;
        test_token_seq lex#l tl ~i:(i + 1);
    )
;;

let test_next_token () =
    let code = "=+(){},;" in

    [   Token.ASSIGN;
        Token.PLUS;
        Token.LPAREN;
        Token.RPAREN;
        Token.LSQUIRLY;
        Token.RSQUIRLY;
        Token.COMMA;
        Token.SEMICOLON;
        Token.EOF
    ] |> test_token_seq (Lexer.create ~input:code)
;;


let test_ident_tokens () =
    let code = "
        let five = 5;
        let ten = 10;

        let result = add(five, ten);
    " 
    in

    [   Token.LET;
        Token.IDENT("five");
        Token.ASSIGN;
        Token.INT("5");
        Token.SEMICOLON;

        Token.LET;
        Token.IDENT("ten");
        Token.ASSIGN;
        Token.INT("10");
        Token.SEMICOLON;

        Token.LET;
        Token.IDENT("result");
        Token.ASSIGN;
        Token.IDENT("add");
        Token.LPAREN;
        Token.IDENT("five");
        Token.COMMA;
        Token.IDENT("ten");
        Token.RPAREN;
        Token.SEMICOLON;
        Token.EOF
    ] |> test_token_seq (Lexer.create ~input:code)
;;

let test_operators () =
    let code = {|
        {};
        
        []$!-/*5\~`?'"%@^#;

        5 < 10 > 5;
    |}
    in

    [   Token.LSQUIRLY;
        Token.RSQUIRLY;
        Token.SEMICOLON;

        Token.LBRACK;
        Token.RBRACK;
        Token.DOLLAR;
        Token.BANG;
        Token.MINUS;
        Token.FORSLASH;
        Token.ASTERISK;
        Token.INT("5");
        Token.BACKSLASH;
        Token.TILDE;
        Token.BACKTICK;
        Token.QUESTION;
        Token.SINGLEQUOTE;
        Token.DOUBLEQUOTE;
        Token.MODULO;
        Token.AT;
        Token.CARET;
        Token.POUND;
        Token.SEMICOLON;

        Token.INT("5");
        Token.LESSER;
        Token.INT("10");
        Token.GREATER;
        Token.INT("5");
        Token.SEMICOLON;
        Token.EOF
    ] |> test_token_seq (Lexer.create ~input:code)
;;


let test_comp_ops () =
    let code = "
        let x = 12;
        let y = 1 =>;
        5 <= 10;
        x >= 2;
        y != x;
        y == 1;
    "
    in

    [   Token.LET;
        Token.IDENT("x");
        Token.ASSIGN;
        Token.INT("12");
        Token.SEMICOLON;

        Token.LET;
        Token.IDENT("y");
        Token.ASSIGN;
        Token.INT("1");
        Token.FATARROW;
        Token.SEMICOLON;

        Token.INT("5");
        Token.LESSEREQ;
        Token.INT("10");
        Token.SEMICOLON;

        Token.IDENT("x");
        Token.GREATEREQ;
        Token.INT("2");
        Token.SEMICOLON;

        Token.IDENT("y");
        Token.NOTEQ;
        Token.IDENT("x");
        Token.SEMICOLON;

        Token.IDENT("y");
        Token.EQUALS;
        Token.INT("1");
        Token.SEMICOLON;
        Token.EOF
    ] |> test_token_seq (Lexer.create ~input:code)
;;

let test_keywords () =
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
    "
    in

    [   Token.LET;
        Token.IDENT("y");
        Token.ASSIGN;
        Token.TRUE;
        Token.SEMICOLON;

        Token.BIND;
        Token.IDENT("x");
        Token.ASSIGN;
        Token.MATCH;
        Token.IDENT("y");
        Token.LSQUIRLY;

        Token.PIPE;
        Token.TRUE;
        Token.FATARROW;
        Token.INT("20");

        Token.PIPE;
        Token.FALSE;
        Token.FATARROW;
        Token.INT("10");

        Token.RSQUIRLY;
        Token.SEMICOLON;

        Token.IF;
        Token.IDENT("x");
        Token.GREATER;
        Token.INT("5");
        Token.LSQUIRLY;

        Token.RETURN;
        Token.INT("2");
        Token.SEMICOLON;

        Token.RSQUIRLY;
        Token.ELSE;
        Token.IF;
        Token.IDENT("x");
        Token.LESSER;
        Token.INT("5");
        Token.LSQUIRLY;

        Token.INT("1");

        Token.RSQUIRLY;
        Token.ELSE;
        Token.LSQUIRLY;

        Token.INT("0");
        
        Token.RSQUIRLY;

        Token.EOF
    ] |> test_token_seq (Lexer.create ~input:code)
;;

let test_functions () =
    let code = "
        fn add (x, y) -> int {
            x + y
        };
    "
    in

    [   Token.FN;
        Token.IDENT("add");
        Token.LPAREN;
        Token.IDENT("x");
        Token.COMMA;
        Token.IDENT("y");
        Token.RPAREN;
        Token.SLIMARROW;
        Token.IDENT("int");
        Token.LSQUIRLY;

        Token.IDENT("x");
        Token.PLUS;
        Token.IDENT("y");

        Token.RSQUIRLY;
        Token.SEMICOLON;

        Token.EOF
    ] |> test_token_seq (Lexer.create ~input:code)
;;
