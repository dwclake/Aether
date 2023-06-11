open Lib
open Alcotest

let tt = testable Token.pp Token.equal

let rec test_token_seq (p: Parser.t) (i: int) = function
    | [] -> ()
    | etok :: tail ->
        check tt (string_of_int i) etok p.cur_t;
        test_token_seq (Parser.next_token(p)) (i + 1) tail;
;;

let test_next_token () =
    let code = "=+(){},;" in

    let tests = [
        Token.ASSIGN;
        Token.PLUS;
        Token.LPAREN;
        Token.RPAREN;
        Token.LSQUIRLY;
        Token.RSQUIRLY;
        Token.COMMA;
        Token.SEMICOLON;
        Token.EOF;
    ] 
    in
    test_token_seq
        (Parser.create (Lexer.create ~input:code))
        1
        tests
;;
