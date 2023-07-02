type option_t = Token.t option
[@@deriving show, eq];;

class t = fun ~lexer -> 
    let current = Lexer.next_token lexer in
    let peek = Lexer.next_token lexer in
    object
        val lexer': Lexer.t ref = lexer
        method lexer = lexer'
        
        val mutable current': option_t = Some current
        method current = current'
        method set_current = fun tok -> current' <- tok
        
        val mutable peek': option_t = Some peek
        method peek = peek'
        method set_peek = fun tok -> peek' <- tok
    end
;;

let next_token ?(count=1) parser =
    let rec next_token' = function
        | x when x <= 0 -> ()
        | x ->
            let token = Lexer.next_token !parser#lexer in

            !parser#set_current !parser#peek;
            !parser#set_peek (Some token);
            next_token' (x - 1)
    in
    next_token' count;
;;

let peek_error parser token =
    Format.sprintf 
        "Expected next token to be %s, got %s instead" 
        (Token.show token) 
        (Token.show (Option.get !parser#peek))
;;

type precedence = 
    [ `Lowest
    | `Equals
    | `LessGreater
    | `Sum
    | `Product
    | `Prefix
    | `Call
    | `Index
][@@deriving ord];;

let comp_prec a b = compare_precedence a b >= 0;;

let get_prec token: precedence =
    match token with
        | Some Token.Eq
        | Some Token.Neq -> `Equals
        | Some Token.Lt
        | Some Token.Leq
        | Some Token.Gt
        | Some Token.Geq -> `LessGreater
        | Some Token.Plus
        | Some Token.Minus -> `Sum
        | Some Token.Slash
        | Some Token.Asterisk -> `Product
        | _ -> `Lowest
;;

let rec parse_statement parser =
    match !parser#current with
        | Some Token.Let
        | Some Token.Const -> parse_binding_statement parser;
        | Some Token.Return -> parse_return_statement parser;
        | _ -> parse_expression_statement parser;

and parse_binding_statement parser =
    let current = Option.get !parser#current in
    match !parser#peek with
        | Some (Token.Ident name) -> begin
            next_token parser;
            match !parser#peek with
                | Some Token.Assign ->
                    next_token parser ~count:2;
                    let expr = parse_expression parser `Lowest in
                    begin match expr with 
                        | Ok value ->
                            Ok (Ast.Binding{
                                kind=current;
                                name;
                                value
                            })
                        | Error message -> Error message
                    end
                    | _ -> Error (peek_error parser Token.Assign)
        end
        | _ -> Error (peek_error parser (Token.Ident ""))

and parse_return_statement parser =
    next_token parser;
    let expr = parse_expression parser `Lowest in

    match expr with 
        | Ok value -> Ok (Ast.Return{value})
        | Error message -> Error message

and parse_expression_statement parser =
    let expr = parse_expression parser `Lowest in
    if !parser#peek == Some Token.Semicolon
        then next_token parser;
    
    match expr with
        | Ok(value) -> Ok (Ast.Expression{value})
        | Error(message) -> Error(message)

and parse_expression parser precedence =
    match get_prefix_fn parser with
        | Some fn ->
            let prefix = fn parser in
            begin match prefix with
                | Ok lhs -> build_infix precedence parser lhs
                | err ->  err
            end
        | None -> Error (Format.sprintf
            "No prefix function for %s"
            (Token.to_string (Option.get !parser#current)))

and get_prefix_fn parser =
    match !parser#current with
        | Some (Token.Ident identifier) -> Some (parse_identifier ~identifier)
        | Some (Token.Int number) -> Some (parse_int ~number)
        | Some (Token.Float number) -> Some (parse_float ~number)
        | Some Token.True -> Some (parse_boolean ~boolean:true)
        | Some Token.False -> Some (parse_boolean ~boolean:false)
        | Some Token.Bang
        | Some Token.Minus -> Some parse_prefix
        | Some Token.Lparen -> Some parse_group
        | Some Token.Lbrace -> Some parse_block
        | Some Token.If -> Some parse_if
        | Some Token.Percent -> Some parse_fn_anon
        | _ -> None

and parse_identifier _ ~identifier =
    Ok (Ast.Identifier identifier)

and parse_int _ ~number = 
    match int_of_string_opt number with
        | Some value -> Ok (Ast.Integer value)
        | None -> Error (Format.sprintf
            "Unable to convert %s to int"
            number
        )

and parse_float _ ~number = 
    match float_of_string_opt number with
        | Some value -> Ok (Ast.Float value)
        | None -> Error (Format.sprintf
            "Unable to convert %s to float"
            number)

and parse_boolean _ ~boolean =
    Ok (Ast.Boolean boolean)

and parse_prefix parser =
    let operator = Option.get !parser#current in
    next_token parser;
    let expr = parse_expression parser `Prefix in
    match expr with 
        | Ok value -> Ok (Ast.Prefix{operator; value})
        | err -> err

and parse_group parser =
    next_token parser;
    match !parser#current with
        | Some Token.Rparen -> 
            next_token parser;
            Ok Ast.Unit
        | _ ->
            let expr = parse_expression parser `Lowest in
            match expr with
                | Ok expr ->
                    begin match !parser#peek with
                        | Some Token.Rparen ->
                            next_token parser;
                            Ok expr
                        | Some _ -> Error (peek_error parser Token.Rparen)
                        | None -> Error "No peek token"
                    end
                | err -> err

and parse_block parser =
    next_token parser;

    let rec parse_block' ?(acc=[]) parser =
        match !parser#current with
            | Some Token.Rbrace ->
                next_token parser;
                Ok acc
            | Some Token.Semicolon ->
                next_token parser;
                parse_block' parser ~acc
            | Some _ ->
                let stmt = parse_statement parser in
                begin match stmt with
                    | Ok stmt -> 
                        next_token parser;
                        parse_block' parser ~acc:([stmt] @ acc)
                    | Error message -> Error message
                end
            | None -> Error "Missing token"
    in
    match !parser#current with
        | Some Token.Rbrace ->
            next_token parser;
            Ok (Ast.Block [Ast.Expression{value= Ast.Unit}])
        | _ ->
            let block = parse_block' parser in
            match block with
                | Ok block -> Ok (Ast.Block (block |> List.rev))
                | Error message -> Error message

and parse_if parser =
    next_token parser;
    let cond = parse_expression parser `Lowest in
    match cond with
        | Ok cond ->
            begin match !parser#peek with
                | Some Token.Lbrace ->
                    next_token parser;
                    let cons = parse_expression parser `Lowest in
                    begin match cons with
                        | Ok cons -> parse_else parser cond cons
                        | Error message -> Error message
                    end
                | Some _ -> Error (peek_error parser Token.Lbrace)
                | None -> Error "No peek token"
            end
        | err -> err

and parse_else parser cond cons =
    match !parser#current with
        | Some Token.Else ->
            next_token parser;
            let alt = parse_expression parser `Lowest in
            begin match alt with
                | Ok alt -> Ok (Ast.If{
                    condition= cond;
                    consequence= cons;
                    alternative= Some alt;
                })
                | Error message -> Error message
            end
        | _ -> Ok (Ast.If{
            condition= cond;
            consequence= cons;
            alternative= None;
        })

and parse_fn_anon parser =
    match !parser#peek with 
        | Some Token.Lbrace ->
            let params = parse_param_list parser in
            begin match params with
                | Ok params ->
                    begin match !parser#peek with
                        | Some Token.Arrow ->
                            next_token parser ~count:2;
                            let expr = parse_expression parser `Lowest in
                            begin match expr with
                                | Ok expr ->
                                    next_token parser ~count:2;
                                    Ok (Ast.AnonFn{
                                        parameter_list= params;
                                        block= expr;
                                    })
                                | err -> err
                            end
                        | Some _ -> Error (peek_error parser Token.Arrow)
                        | None -> Error "Missing peek token"
                    end
                | Error message -> Error message
            end
        | Some _ -> Error (peek_error parser Token.Lbrace)
        | None -> Error "Missing peek token"

and parse_param_list parser =
    next_token parser ~count:2;

    let rec parse_param_list' ?(acc=[]) parser =
        match !parser#current with
            | Some Token.Ident ident ->
                begin match !parser#peek with
                    | Some Token.Comma -> 
                        next_token parser ~count:2;
                        parse_param_list' parser ~acc:[ident] @ acc
                    | _ -> [ident] @ acc
                end
            | _ -> acc
    in
    let params = parse_param_list' parser in
    Ok (params |> List.rev)

and build_infix precedence parser lhs =
    match !parser#peek with
        | x when comp_prec precedence (get_prec x) -> Ok lhs
        | _ ->
            match get_infix_fn parser with
                | Some fn ->
                    let expr = fn lhs in
                    begin match expr with
                        | Ok expr -> build_infix precedence parser expr
                        | err -> err
                    end
                | None -> Ok lhs

and get_infix_fn parser =
    match !parser#peek with
        | Some Token.Plus
        | Some Token.Minus
        | Some Token.Slash
        | Some Token.Asterisk
        | Some Token.Eq
        | Some Token.Neq
        | Some Token.Lt
        | Some Token.Gt
        | Some Token.Leq
        | Some Token.Geq
        | Some Token.Lparen
        | Some Token.Lbracket -> 
            next_token parser;
            Some (parse_infix parser)
        | _ -> None

and parse_infix parser lhs =
    let operator = Option.get !parser#current in
    let precedence = get_prec !parser#current in
    
    next_token parser;
    let rhs = parse_expression parser precedence in
    match rhs with
        | Ok rhs -> 
            Ok (Ast.Infix{
                lhs;
                operator;
                rhs
            })
        | err -> err
;;

let parse_program parser: Ast.program =
    let rec parse_program' parser stmts errors =
        match !parser#current with
            | Some Token.Eof -> (stmts, errors)
            | _ ->
                let stmt = parse_statement parser in
                next_token parser;
                match stmt with
                    | Ok stmt -> 
                        parse_program' parser ([stmt] @ stmts) errors
                    | Error message -> 
                        parse_program' parser stmts ([message] @ errors)
    in
    let (statements, errors) = parse_program' parser [] [] in

    let statements = statements |> List.rev in
    let errors = errors |> List.rev in

    {statements; errors}
;;
