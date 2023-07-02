open Aether
open Alcotest

let tt = testable Token.pp Token.equal

let rec test_token_seq lexer ?(i=1) = function
    | [] -> ()
    | (et::tl) -> (
        let token = Lexer.next_token lexer in

        check tt (string_of_int i) et token;
        test_token_seq lexer tl ~i:(i + 1);
    )
;;

let test_next_token () =
    let code = "=+(){},;" in

    [   Token.Assign;
        Token.Plus;
        Token.Lparen;
        Token.Rparen;
        Token.Lbrace;
        Token.Rbrace;
        Token.Comma;
        Token.Semicolon;
        Token.Eof;
    ]
    |> test_token_seq (new Lexer.t ~input:code |> ref)
;;


let test_ident_tokens () =
    let code = "
        let five = 5;
        let ten = 10.0;

        let result = add(five, ten);
    " 
    in

    [   Token.Let;
        Token.Ident "five";
        Token.Assign;
        Token.Int "5";
        Token.Semicolon;

        Token.Let;
        Token.Ident "ten";
        Token.Assign;
        Token.Float "10.0";
        Token.Semicolon;

        Token.Let;
        Token.Ident "result";
        Token.Assign;
        Token.Ident "add";
        Token.Lparen;
        Token.Ident "five";
        Token.Comma;
        Token.Ident "ten";
        Token.Rparen;
        Token.Semicolon;
        Token.Eof;
    ] 
    |> test_token_seq (new Lexer.t ~input:code |> ref)
;;

let test_operators () =
    let code = {|
        {};
        
        []:.$!-/*5\~`?'"%@^#;

        5 < 10 > 5;
    |}
    in

    [   Token.Lbrace;
        Token.Rbrace;
        Token.Semicolon;

        Token.Lbracket;
        Token.Rbracket;
        Token.Colon;
        Token.Dot;
        Token.Dollar;
        Token.Bang;
        Token.Minus;
        Token.Slash;
        Token.Asterisk;
        Token.Int "5";
        Token.Backslash;
        Token.Tilde;
        Token.Backtick;
        Token.Question;
        Token.Quote;
        Token.DoubleQuote;
        Token.Percent;
        Token.Address;
        Token.Caret;
        Token.Pound;
        Token.Semicolon;

        Token.Int "5";
        Token.Lt;
        Token.Int "10";
        Token.Gt;
        Token.Int "5";
        Token.Semicolon;
        Token.Eof;
    ] 
    |> test_token_seq (new Lexer.t ~input:code |> ref)
;;


let test_comp_ops () =
    let code = "
        let x = 12;
        let y = 1 =>;
        5 <= 10;
        x >= 2;
        y != x;
        y == 1;
        true || false;
        x && y;
    "
    in

    [   Token.Let;
        Token.Ident "x";
        Token.Assign;
        Token.Int "12";
        Token.Semicolon;

        Token.Let;
        Token.Ident "y";
        Token.Assign;
        Token.Int "1";
        Token.FatArrow;
        Token.Semicolon;

        Token.Int "5";
        Token.Leq;
        Token.Int "10";
        Token.Semicolon;

        Token.Ident "x";
        Token.Geq;
        Token.Int "2";
        Token.Semicolon;

        Token.Ident "y";
        Token.Neq;
        Token.Ident "x";
        Token.Semicolon;

        Token.Ident "y";
        Token.Eq;
        Token.Int "1";
        Token.Semicolon;

        Token.True;
        Token.Or;
        Token.False;
        Token.Semicolon;

        Token.Ident "x";
        Token.And;
        Token.Ident "y";
        Token.Semicolon;

        Token.Eof;
    ] 
    |> test_token_seq (new Lexer.t ~input:code |> ref)
;;

let test_keywords () =
    let code = "
        let y = true;
        const x = match y {
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

    [   Token.Let;
        Token.Ident "y";
        Token.Assign;
        Token.True;
        Token.Semicolon;

        Token.Const;
        Token.Ident "x";
        Token.Assign;
        Token.Match;
        Token.Ident "y";
        Token.Lbrace;

        Token.Pipe;
        Token.True;
        Token.FatArrow;
        Token.Int "20";

        Token.Pipe;
        Token.False;
        Token.FatArrow;
        Token.Int "10";

        Token.Rbrace;
        Token.Semicolon;

        Token.If;
        Token.Ident "x";
        Token.Gt;
        Token.Int "5";
        Token.Lbrace;

        Token.Return;
        Token.Int "2";
        Token.Semicolon;

        Token.Rbrace;
        Token.Else;
        Token.If;
        Token.Ident "x";
        Token.Lt;
        Token.Int "5";
        Token.Lbrace;

        Token.Int "1";

        Token.Rbrace;
        Token.Else;
        Token.Lbrace;

        Token.Int "0";
        
        Token.Rbrace;

        Token.Eof;
    ] 
    |> test_token_seq (new Lexer.t ~input:code |> ref)
;;

let test_functions () =
    let code = "
        let add = fn(x, y) -> int {
            x + y
        };
    "
    in

    [   Token.Let;
        Token.Ident "add";
        Token.Assign;
        Token.Fn;
        Token.Lparen;
        Token.Ident "x";
        Token.Comma;
        Token.Ident "y";
        Token.Rparen;
        Token.Arrow;
        Token.Ident "int";
        Token.Lbrace;

        Token.Ident "x";
        Token.Plus;
        Token.Ident "y";

        Token.Rbrace;
        Token.Semicolon;

        Token.Eof;
    ] 
    |> test_token_seq (new Lexer.t ~input:code |> ref)
;;
