open Briar
open Alcotest

let tt = testable Parser.pp_option_t Parser.equal_option_t
let ts = testable Ast.pp_statement Ast.equal_statement

let unwrap result = match result with
    | Ok (_, program) -> program
    | Error (_, message) -> failwith message
;;

let test_stmts_length (program: Ast.program) len =
    if List.length program.statements != len 
        then failf "statements length is not %d. got=%d" 
            len 
            @@ List.length program.statements
;;

let rec test_statement_seq ?(i=1) lists = match lists with
    | ([], []) -> ()
    | (es::et, s::t) ->
        check ts (string_of_int i) es s;
        check Alcotest.string (string_of_int i) "statement" (Ast.token_literal (Ast.Statement s));

        test_statement_seq (et, t) ~i:(i + 1);
    | _ -> failwith "Lists must be of the same size"
;;

let test_binding_statement () =
    let program = "
        let x = 5;
        const y = 10;
        let foobar = 838383;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
    in
    let () = test_stmts_length program 3 in

    ([ Ast.Binding{kind= Token.Let; name= {identifier= "x"}; value= Ast.Integer 5}
     ; Ast.Binding{kind= Token.Const; name= {identifier= "y"}; value= Ast.Integer 10}
     ; Ast.Binding{kind= Token.Let; name= {identifier= "foobar"}; value= Ast.Integer 838383}
     ],
       program.statements
    ) 
    |> test_statement_seq
;;

let test_return_statement () =
    let program = "
        return ();
        return {};
        return 10;
        return 993322;
        "
        |> new Lexer.t 
        |> ref 
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
    in
    let () = test_stmts_length program 4 in

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
    let program = "
        foobar;
        "
        |> new Lexer.t 
        |> ref 
        |> Parser.create
        |> Parser.parse_program
        |> unwrap
    in
    let () = test_stmts_length program 1 in

    ([ Ast.Expression{value= Ast.Identifier {identifier= "foobar"}}],
       program.statements
    )
    |> test_statement_seq
;;

let test_integer_expression () =
    let program = "
        5;
        "
        |> new Lexer.t
        |> ref
        |> Parser.create
        |> Parser.parse_program
        |> unwrap
    in
    let () = test_stmts_length program 1 in

    ([ Ast.Expression{value= Ast.Integer 5}],
       program.statements
    )
    |> test_statement_seq
;;

let test_float_expression () =
    let program = "
        5.4;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
    in
    let () = test_stmts_length program 1 in

    ([ Ast.Expression{value= Ast.Float 5.4}],
       program.statements
    )
    |> test_statement_seq
;;

let test_boolean_expression () =
    let program = "
        true;
        !false;
        const foobar = true;
        let barfoo = false;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
    in
    let () = test_stmts_length program 4 in

    ([ Ast.Expression{value= Ast.Boolean true}
     ; Ast.Expression{value= Ast.Prefix{operator= Token.Bang; value= Ast.Boolean false}}
     ; Ast.Binding{kind= Token.Const; name= {identifier= "foobar"}; value= Ast.Boolean true}
     ; Ast.Binding{kind= Token.Let; name= {identifier= "barfoo"}; value= Ast.Boolean false}
     ],
       program.statements
    )
    |> test_statement_seq
;;

let test_prefix_expression () =
    let program = "
        !5;
        -15;
        !foobar;
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
    in
    let () = test_stmts_length program 3 in

    ([ Ast.Expression{value= Ast.Prefix{operator= Token.Bang; value= Ast.Integer 5}}
     ; Ast.Expression{value= Ast.Prefix{operator= Token.Minus; value= Ast.Integer 15}}
     ; Ast.Expression{value= Ast.Prefix{operator= Token.Bang; value= Ast.Identifier {identifier= "foobar"}}}
     ],
       program.statements
    )
    |> test_statement_seq
;;

let test_infix_expression () =
    let program = "
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
        |> unwrap
    in
    let () = test_stmts_length program 8 in

    ([ Ast.Expression{value= Ast.Infix
            { lhs= Ast.Integer 5
            ; operator= Token.Plus
            ; rhs= Ast.Identifier {identifier= "foobar"}
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Identifier {identifier= "bar"}
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
            { lhs= Ast.Identifier {identifier= "a"}
            ; operator= Token.Plus
            ; rhs= Ast.Infix{lhs= Ast.Identifier {identifier= "b"}; operator= Token.Slash; rhs= Ast.Identifier {identifier= "c"}}
            }}
     ; Ast.Expression{value= Ast.Infix
            { lhs= Ast.Prefix{operator= Token.Minus; value= Ast.Integer 5}
            ; operator= Token.Asterisk
            ; rhs= Ast.Prefix{operator= Token.Bang; value= Ast.Identifier {identifier= "x"}}
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
    let program = "
        if x < y { x } 
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
    in
    let () = test_stmts_length program 1 in

    ([ Ast.Expression{value= Ast.If
            { condition= Ast.Infix
                { lhs= Ast.Identifier {identifier= "x"}
                ; operator= Token.Lt
                ; rhs= Ast.Identifier {identifier= "y"}
                }
            ; consequence= Ast.Block [Ast.Expression{value= Ast.Identifier {identifier= "x"}}]
            ; alternative= None
            }}
     ],
       program.statements    
    )
    |> test_statement_seq
;;

let test_if_else_expression () =
    let program = "
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
        |> unwrap
    in
    let () = test_stmts_length program 2 in

    ([ Ast.Expression{value= Ast.If
            { condition= Ast.Infix
                { lhs= Ast.Identifier {identifier= "x"}
                ; operator= Token.Lt
                ; rhs= Ast.Identifier {identifier= "y"}
                }
            ; consequence= Ast.Block [Ast.Expression{value= Ast.Identifier {identifier= "x"}}]
            ; alternative= Some (Ast.Block [Ast.Expression{value= Ast.Identifier {identifier= "y"}}])
            }}
     ; Ast.Expression{value= Ast.If
            { condition= Ast.Infix
                { lhs= Ast.Identifier {identifier= "x"}
                ; operator= Token.Lt
                ; rhs= Ast.Identifier {identifier= "y"}
                }
            ; consequence= Ast.Block [Ast.Expression{value= Ast.Unit}]
            ; alternative= Some (Ast.Block [Ast.Expression{value= Ast.Identifier {identifier= "y"}}])
            }}
     ],
       program.statements    
    )
    |> test_statement_seq
;;

let test_fn_literal_expression () =
    let program = "
        |x, y| => x + y
        |x, y| => {x + y};
        |foo| => {
            x;
            12
        };
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program 
        |> unwrap
    in
    let () = test_stmts_length program 3 in

    ([ Ast.Expression{value= Ast.AnonFn
            { parameters= [{identifier= "x"}; {identifier= "y"}]
            ; block= Ast.Infix
                { lhs= Ast.Identifier {identifier= "x"}
                ; operator= Token.Plus
                ; rhs= Ast.Identifier {identifier= "y"}
                }
            ; arity= 2
            }}
     ; Ast.Expression{value= Ast.AnonFn
            { parameters= [{identifier= "x"}; {identifier= "y"}]
            ; block= Ast.Block [Ast.Expression{value= Ast.Infix
                { lhs= Ast.Identifier {identifier= "x"}
                ; operator= Token.Plus
                ; rhs= Ast.Identifier {identifier= "y"}
                }}]
            ; arity= 2
            }}
     ; Ast.Expression{value= Ast.AnonFn
            { parameters= [{identifier= "foo"}]
            ; block= Ast.Block
                [ Ast.Expression{value= Ast.Identifier {identifier= "x"}}
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
    let program = "
        let sum = |x, y| => x + y;
        sum(1, 2);
        sum();
        sum()
        "
        |> new Lexer.t 
        |> ref
        |> Parser.create
        |> Parser.parse_program
        |> unwrap
    in
    let () = test_stmts_length program 4 in

    ([ Ast.Binding
            { kind= Token.Let
            ; name= {identifier= "sum"}
            ; value= Ast.AnonFn
                { parameters= [{identifier= "x"}; {identifier= "y"}]
                ; block= Ast.Infix
                    { lhs= Ast.Identifier {identifier= "x"}
                    ; operator= Token.Plus
                    ; rhs= Ast.Identifier {identifier= "y"}
                    }
                ; arity= 2
                }}
     ; Ast.Expression{value= Ast.FnCall
            { fn= Ast.Identifier {identifier= "sum"}
            ; arguments= [Ast.Integer 1; Ast.Integer 2]
            }}
     ; Ast.Expression{value= Ast.FnCall
            { fn= Ast.Identifier {identifier= "sum"}
            ; arguments= [Ast.Unit]
            }}
     ; Ast.Expression{value= Ast.FnCall
            { fn= Ast.Identifier {identifier= "sum"}
            ; arguments= [Ast.Unit]
            }}
     ],
       program.statements
    )
    |> test_statement_seq
;;

let test_complex_parsing () =
    let program = "
        const div = |x, y| => {
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
        |> unwrap
    in
    let () = test_stmts_length program 1 in

    ([ Ast.Binding
            { kind= Token.Const
            ; name= {identifier= "div"}
            ; value= Ast.AnonFn
                {parameters= [{identifier= "x"}; {identifier= "y"}]
                ; block= Ast.Block [
                    Ast.Expression{value= Ast.If
                        { condition= Ast.Infix
                            { lhs= Ast.Identifier {identifier= "y"}
                            ; operator= Token.Neq
                            ; rhs= Ast.Integer 0
                            }
                        ; consequence= Ast.Block [Ast.Expression{value= Ast.Infix
                            { lhs= Ast.Identifier {identifier= "x"}
                            ; operator= Token.Slash
                            ; rhs= Ast.Identifier {identifier= "y"}
                            }}]
                        ; alternative= Some (Ast.Block [Ast.Expression{value= Ast.Infix
                            { lhs= Ast.Identifier {identifier= "x"}
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
