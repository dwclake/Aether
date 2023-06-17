open Aether
open Alcotest

let tt = testable Token.pp Token.equal
let ti = testable Ast.pp_identifier Ast.equal_identifier
let ts = testable Ast.pp_statement Ast.equal_statement
let tstr = Alcotest.string

let check_parser_errors (p: Parser.t) =
    let open List in
    let open Stdio in

    if length p.errors == 0 then
        ()
    else
        eprintf "\nParser had %d errors" (length p.errors);
        let rec print = function
            | [] -> eprintf "\n\nend of parser error list"
            | h::t ->
                eprintf "\n- parser error: %s" h;
                print t
        in
        print p.errors
;;

let rec test_token_seq (p: Parser.t) ?(i = 1) = function
    | [] -> ()
    | et::tl ->
        check tt (string_of_int i) et p.cur_t;
        test_token_seq (Parser.next_token p) tl ~i:(i + 1);
;;

let rec test_statement_seq ?(i = 1) (l: (Ast.identifier list * Ast.node list)) =
    match l with
    | ([], []) -> ()
    | (eid::etl, nd::tl) ->
        let stmt = match nd with
            | STATEMENT s -> s
            | _ -> failwith "Node should be a statement"
        in
        let estmt = Ast.LET{name= eid; value= IDENTIFIER eid} in

        let (ename, sname) = match (estmt, stmt) with
            | (LET e, LET s) -> (e.name, s.name)
            | _ -> failwith "Statement should be a let statement"
        in

        check ti (string_of_int i) ename sname;
        check ts (string_of_int i) estmt stmt;
        check tstr (string_of_int i) "statement" (Ast.token_literal nd);
        test_statement_seq (etl, tl) ~i:(i + 1);
    | _ -> ()
;;

let test_next_token () =
    let code = "=+(){},;" in

    let l = Lexer.create ~input:code in
    let p = Parser.create l in

    [   Token.ASSIGN;
        Token.PLUS;
        Token.LPAREN;
        Token.RPAREN;
        Token.LSQUIRLY;
        Token.RSQUIRLY;
        Token.COMMA;
        Token.SEMICOLON;
        Token.EOF;
    ] 
    |> test_token_seq p
;;

let test_let_statement () =
    let code = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    "
    in
    let l = Lexer.create ~input:code in
    let p = Parser.create l in

    let (p, program) = Parser.parse_program p in
    let () = check_parser_errors p in

    if List.length program.statements != 3 then ( 
        failwith "Program statements list length is incorrect";
    );

    ([  {identifier="x"};
        {identifier="y"};
        {identifier="foobar"};
     ],
        program.statements
    ) 
    |> test_statement_seq
;;

let test_return_statement () =
    let code = "
        return 5;
        return 10;
        return 993322;
    "
    in

    let l = Lexer.create ~input:code in
    let p = Parser.create l in

    let (p, program) = Parser.parse_program p in
    let () = check_parser_errors p in

    if List.length program.statements != 3 then
        failwith "Program statements list length is incorrect";

    let rec loop = function
        | [] -> ()
        | h::t ->
            match h with
            | Ast.STATEMENT s -> begin
                    match s with
                    | Ast.RETURN _ -> loop t
                    | _ as stmt ->
                        failwith (
                            Format.sprintf
                            "Not a return statement, got %s"
                            (Ast.show_statement stmt)
                        )
            end
            | _ as nd ->
                failwith (
                    Format.sprintf
                    "Not a statement node, got %s"
                    (Ast.show_node nd)
                )
    in
    loop program.statements
;;
