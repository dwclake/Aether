open Briar
open Alcotest

let tt = testable Parser.pp_option_t Parser.equal_option_t
let ts = testable Ast.pp_statement Ast.equal_statement

let check_parser_errors list =
    let open List in
    let open Stdio in

    if length list == 0 then
        ()
    else
        eprintf "Parser had %d errors" (length list);
        let rec print = function
            | [] -> eprintf "\n"; flush_all();
            | h::t ->
                eprintf "\n- parser error: %s" h;
                print t
        in
        print list
;;

let rec test_statement_seq ?(i=1) lists = match lists with
    | ([], []) -> ()
    | (es::et, s::t) ->
        check ts (string_of_int i) es s;
        check Alcotest.string (string_of_int i) "statement" (Ast.token_literal (Ast.Statement s));

        test_statement_seq (et, t) ~i:(i + 1);
    | _ -> failwith "Lists must be of the same size"
;;

let test_stmts_length (program: Ast.program) len =
    if List.length program.statements != len 
        then failf "statements length is not %d. got=%d" 
            len 
            @@ List.length program.statements
;;

let test_binding_statement () =
    let _, program = "
        let x = 5;
        const y = 10;
        let foobar = 838383;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 3;

    ([ Ast.Binding{kind= Token.Let; name= "x"; value= Ast.Integer 5}
     ; Ast.Binding{kind= Token.Const; name= "y"; value= Ast.Integer 10}
     ; Ast.Binding{kind= Token.Let; name= "foobar"; value= Ast.Integer 838383}
     ],
        program.statements
    ) 
    |> test_statement_seq
;;

let test_return_statement () =
    let _, program = "
        return ();
        return {};
        return 10;
        return 993322;
        "
        |> new Lexer.t 
        |> ref 
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 4;
    
    ([ Ast.Return{value= Ast.Unit}
     ; Ast.Return{value= Ast.Block [Ast.Expression{value= Ast.Unit}]}
     ; Ast.Return{value= Ast.Integer 10}
     ; Ast.Return{value= Ast.Integer 993322}
    ],
        program.statements
    )
    |> test_statement_seq
;;

let test_identifier_expression () =
    let _, program = "
        foobar;
        "
        |> new Lexer.t 
        |> ref 
        |> Parser.create
        |> Parser.parse_program
    in
    check_parser_errors program.errors;
    test_stmts_length program 1;

    ([ Ast.Expression{value= Ast.Identifier "foobar"}],
       program.statements
    )
    |> test_statement_seq
;;

let test_integer_expression () =
    let _, program = "
        5;
        "
        |> new Lexer.t
        |> ref
        |> Parser.create
        |> Parser.parse_program
    in
    check_parser_errors program.errors;
    test_stmts_length program 1;

    ([ Ast.Expression{value= Ast.Integer 5}],
       program.statements
    )
    |> test_statement_seq
;;

let test_float_expression () =
    let _, program = "
        5.4;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 1;

    ([ Ast.Expression{value= Ast.Float 5.4}],
       program.statements
    )
    |> test_statement_seq
;;

let test_boolean_expression () =
    let _, program = "
        true;
        !false;
        const foobar = true;
        let barfoo = false;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 4;

    ([ Ast.Expression{value= Ast.Boolean true}
     ; Ast.Expression{value= Ast.Prefix{operator= Token.Bang; value= Ast.Boolean false}}
     ; Ast.Binding{kind= Token.Const; name= "foobar"; value= Ast.Boolean true}
     ; Ast.Binding{kind= Token.Let; name= "barfoo"; value= Ast.Boolean false}
     ],
        program.statements
    )
    |> test_statement_seq
;;

let test_prefix_expression () =
    let _, program = "
        !5;
        -15;
        !foobar;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 3;

    ([ Ast.Expression{value= Ast.Prefix{operator= Token.Bang; value= Ast.Integer 5}}
     ; Ast.Expression{value= Ast.Prefix{operator= Token.Minus; value= Ast.Integer 15}}
     ; Ast.Expression{value= Ast.Prefix{operator= Token.Bang; value= Ast.Identifier "foobar"}}
    ],
        program.statements
    )
    |> test_statement_seq
;;

let test_infix_expression () =
    let _, program = "
        5 + foobar;
        bar / 12;
        12.2 * 13;
        15 >= 13;
        a + b / c;
        -5 * !x;
        3 > 5 == false;
        1 + (2 + 3) + 4;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 8;

    ([ Ast.Expression{value= Ast.Infix
            { lhs= Ast.Integer 5
            ; operator= Token.Plus
            ; rhs= Ast.Identifier "foobar"
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Identifier "bar"
            ; operator= Token.Slash
            ; rhs= Ast.Integer 12
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Float 12.2
            ; operator= Token.Asterisk
            ; rhs= Ast.Integer 13
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Integer 15
            ; operator= Token.Geq
            ; rhs= Ast.Integer 13
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Identifier "a"
            ; operator= Token.Plus
            ; rhs= Ast.Infix{lhs= Ast.Identifier "b"; operator= Token.Slash; rhs= Ast.Identifier "c"}
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Prefix{operator= Token.Minus; value= Ast.Integer 5}
            ; operator= Token.Asterisk
            ; rhs= Ast.Prefix{operator= Token.Bang; value= Ast.Identifier "x"}
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Infix{lhs=Ast.Integer 3 ; operator= Token.Gt; rhs= Ast.Integer 5}
            ; operator= Token.Eq
            ; rhs= Ast.Boolean false
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Infix{lhs= Ast.Integer 1; operator= Token.Plus; rhs= Ast.Infix
                { lhs = Ast.Integer 2
                ; operator = Token.Plus
                ; rhs = Ast.Integer 3
                }}
            ; operator= Token.Plus
            ; rhs= Ast.Integer 4
            }}
    ],
        program.statements
    )
    |> test_statement_seq
;;

let test_if_expression () =
    let _, program = "
        if x < y { x } 
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 1;

    ([ Ast.Expression{value= Ast.If
            { condition= Ast.Infix
                { lhs= Ast.Identifier "x"
                ; operator= Token.Lt
                ; rhs= Ast.Identifier "y"
                }
            ; consequence= Ast.Block [Ast.Expression{value= Ast.Identifier "x"}]
            ; alternative= None
            }}
    ],
        program.statements    
    )
    |> test_statement_seq
;;

let test_if_else_expression () =
    let _, program = "
        if (x < y) { 
            x 
        } else {
            y;
        };
        if (x < y) {
            ();
        } else {
            y
        }
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    
    test_stmts_length program 2;

    ([ Ast.Expression{value= Ast.If
            { condition= Ast.Infix
                { lhs= Ast.Identifier "x"
                ; operator= Token.Lt
                ; rhs= Ast.Identifier "y"
                }
            ; consequence= Ast.Block [Ast.Expression{value= Ast.Identifier "x"}]
            ; alternative= Some (Ast.Block [Ast.Expression{value= Ast.Identifier "y"}])
            }}
     ; Ast.Expression{value= Ast.If
            { condition= Ast.Infix
                { lhs= Ast.Identifier "x"
                ; operator= Token.Lt
                ; rhs= Ast.Identifier "y"
                }
            ; consequence= Ast.Block [Ast.Expression{value= Ast.Unit}]
            ; alternative= Some (Ast.Block [Ast.Expression{value= Ast.Identifier "y"}])
            }}
    ],
        program.statements    
    )
    |> test_statement_seq
;;

let test_fn_literal_expression () =
    let _, program = "
        fn x, y => x + y
        fn x, y => {x + y};
        fn foo => {
            x;
            12
        };
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 3;

    ([ Ast.Expression{value= Ast.AnonFn
            { parameters= ["x"; "y"]
            ; block= Ast.Infix
                { lhs= Ast.Identifier "x"
                ; operator= Token.Plus
                ; rhs= Ast.Identifier "y"
                }
            ; arity= 2
            }}
     ; Ast.Expression{value= Ast.AnonFn
            { parameters= ["x"; "y"]
            ; block= Ast.Block [Ast.Expression{value= Ast.Infix
                { lhs= Ast.Identifier "x"
                ; operator= Token.Plus
                ; rhs= Ast.Identifier "y"
                }}]
            ; arity= 2
            }}
     ; Ast.Expression{value= Ast.AnonFn
            { parameters= ["foo"]
            ; block= Ast.Block 
                [ Ast.Expression{value= Ast.Identifier "x"}
                ; Ast.Expression{value= Ast.Integer 12}
                ]
            ; arity= 1
            }}
    ],
        program.statements
    )
    |> test_statement_seq
;;

let test_fn_call_expression () =
    (*let _, program = "
        let sum = fn x, y => x + y;
        sum(1, 2)
        "
    *)
    let _, program = "
        sum(1, 2)
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 1;

    ([ (*Ast.Binding
            { kind= Token.Let
            ; name= "sum"
            ; value= Ast.AnonFn
                { parameters= ["x"; "y"]
                ; block= Ast.Infix
                    { lhs= Ast.Identifier "x"
                    ; operator= Token.Plus
                    ; rhs= Ast.Identifier "y"
                    }
                ; arity= 2
                }}
     ;*) Ast.Expression{value= Ast.FnCall
            { fn= Ast.Identifier "sum"
            ; arguments= [Ast.Integer 1; Ast.Integer 2]
            }}
    ],
        program.statements
    )
    |> test_statement_seq
;;

let test_complex_parsing () =
    let _, program = "
        const div = fn x, y => {
            if y != 0 {
                x / y;
            } else {
                x / 1
            }
        };
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
    in
    check_parser_errors program.errors;
    test_stmts_length program 1;

    ([ Ast.Binding
            { kind= Token.Const
            ; name= "div"
            ; value= Ast.AnonFn
                {parameters= ["x"; "y"]
                ; block= Ast.Block [
                    Ast.Expression{value= Ast.If
                        { condition= Ast.Infix
                            { lhs= Ast.Identifier "y"
                            ; operator= Token.Neq
                            ; rhs= Ast.Integer 0
                            }
                        ; consequence= Ast.Block [Ast.Expression{value= Ast.Infix
                            { lhs= Ast.Identifier "x"
                            ; operator= Token.Slash
                            ; rhs= Ast.Identifier "y"
                            }}]
                        ; alternative= Some (Ast.Block [Ast.Expression{value= Ast.Infix
                            { lhs= Ast.Identifier "x"
                            ; operator= Token.Slash
                            ; rhs= Ast.Integer 1
                            }}])
                        }}
                ]
                ; arity= 2
                }}
    ],
        program.statements
    )
    |> test_statement_seq
;;
