open Lib
open Alcotest

let tt = testable Token.pp Token.equal
let ti = testable Ast.pp_identifier Ast.equal_identifier

let rec test_token_seq (p: Parser.t) (i: int) = function
    | [] -> ()
    | etk::tl ->
        check tt (string_of_int i) etk p.cur_t;
        test_token_seq (Parser.next_token(p)) (i + 1) tl;
;;

let rec test_statement_seq (i: int) (l: (Ast.identifier list * Ast.statement list)) =
    match l with
    | [], [] -> ()
    | ex::et, s::st -> (
        let ident = match s with
            | LET s -> s.name
            (*| _ -> failwith "Statement is not a LET statement"*)
        in
        check ti (string_of_int i) ex ident;
        test_statement_seq (i + 1) (et, st);
    )
    | _ -> ()
;;

let test_next_token () =
    let code = "=+(){},;" in
    let p = Parser.create (Lexer.create ~input:code) in

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
    test_token_seq p 1 tests
;;

let test_let_statement () =
    let code = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    "
    in
    let p = Parser.create (Lexer.create ~input:code) in

    let program = match Parser.parse_program p with
        | Ok prog -> prog
        | Error e -> failwith e
    in

    if List.length program.statements != 3
        then failwith "Program statements list length is incorrect"
    ;

    let tests: Ast.identifier list = [
        {identifier="x"};
        {identifier="y"};
        {identifier="foobar"};
    ]
    in

    test_statement_seq 1 (tests, program.statements)
;;
