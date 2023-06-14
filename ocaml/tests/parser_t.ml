open Lib
open Alcotest

let tt = testable Token.pp Token.equal
let ti = testable Ast.pp_identifier Ast.equal_identifier

let check_parser_errors (p: Parser.t) =
    let open List in
    let open Stdio in

    if length p.errors == 0 then
        ()
    else
        eprintf "\nParser had %d errors" (length p.errors);
        let rec print = function
            | [] -> eprintf "\n\nend of parser error list"
            | h::t -> ( 
                eprintf "\n- parser error: %s" h;
                print t
            )
        in
        print p.errors
;;



let rec test_token_seq (p: Parser.t) ?(i = 1) = function
    | [] -> ()
    | et::tl -> (
        check tt (string_of_int i) et p.cur_t;
        test_token_seq (Parser.next_token p) tl ~i:(i + 1);
    )
;;

let rec test_statement_seq ?(i = 1) (l: (Ast.identifier list * Ast.statement list)) =
    match l with
    | ([], []) -> ()
    | (ex::et, st::tl) -> (
        let ident = match st with
            | LET s -> s.name
        in
        check ti (string_of_int i) ex ident;
        test_statement_seq (et, tl) ~i:(i + 1);
    )
    | _ -> ()
;;

let test_next_token () =
    let code = "=+(){},;" in
    let p = Parser.create (Lexer.create ~input:code) in
    check_parser_errors p;

    [   Token.ASSIGN;
        Token.PLUS;
        Token.LPAREN;
        Token.RPAREN;
        Token.LSQUIRLY;
        Token.RSQUIRLY;
        Token.COMMA;
        Token.SEMICOLON;
        Token.EOF;
    ] |> test_token_seq p
;;

let test_let_statement () =
    let code = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    "
    in
    let p = Parser.create (Lexer.create ~input:code) in

    let (p, program) = Parser.parse_program p in
    check_parser_errors p;

    if List.length program.statements != 3 then ( 
        failwith "Program statements list length is incorrect";
    );

    ([  {identifier="x"};
        {identifier="y"};
        {identifier="foobar"};
     ],
        program.statements
    ) |> test_statement_seq
;;
