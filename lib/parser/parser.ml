type option_t = Token.t option
[@@deriving show, eq];;

type t = 
    { lexer: Lexer.t ref
    ; current: option_t
    ; peek: option_t;
    };;

let next_token ?(count=1) parser =
    let rec next_token' parser = function
        | x when x <= 0 -> parser
        | x ->
            let token = Lexer.next_token parser.lexer in
            next_token' { lexer= parser.lexer
                        ; current= parser.peek
                        ; peek= Some token
                        } (x - 1)
    in 
    next_token' parser count
;;

let create ~lexer =
    { lexer
    ; current= None
    ; peek= None
    }
    |> next_token ~count:2
;;

let peek_error parser token =
    Format.sprintf 
        "Expected next token to be %s, got %s instead" 
        (Token.show token) 
        (Token.show (Option.get parser.peek))
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

let get_prec token: precedence = match token with
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

let rec parse_statement parser = match parser.current with
    | Some Token.Let -> parse_binding_statement parser Token.Let
    | Some Token.Const -> parse_binding_statement parser Token.Const;
    | Some Token.Return -> parse_return_statement parser;
    | _ -> parse_expression_statement parser;

and parse_binding_statement parser kind = match parser.peek with
    | Some (Token.Ident name) -> begin
        let parser = next_token parser in
        match parser.peek with
            | Some Token.Assign ->
                let parser =next_token parser ~count:2 in
                let parser, expr = parse_expression parser `Lowest in
                begin match expr with 
                    | Ok value ->
                        parser, Ok (Ast.Binding
                            { kind
                            ; name
                            ; value
                            })
                    | Error message -> parser, Error message
                end
                | _ -> parser, Error (peek_error parser Token.Assign)
    end
    | _ -> parser, Error (peek_error parser @@ Token.Ident "")

and parse_return_statement parser =
    let parser = next_token parser in
    let parser, expr = parse_expression parser `Lowest in

    match expr with 
        | Ok value -> parser, Ok (Ast.Return{value})
        | Error message -> parser, Error message

and parse_expression_statement parser =
    let parser, expr = parse_expression parser `Lowest in
    let parser = if parser.peek == Some Token.Semicolon
        then next_token parser
        else parser
    in
    
    match expr with
        | Ok(value) -> parser, Ok (Ast.Expression{value})
        | Error(message) -> parser, Error message

and parse_expression parser precedence = match get_prefix_fn parser with
    | Some fn ->
        let parser, prefix = fn parser in
        begin match prefix with
            | Ok lhs -> build_infix precedence parser lhs
            | err ->  parser, err
        end
    | None -> parser, Error (Format.sprintf
        "No prefix function for %s"
        (Token.to_string @@ Option.get parser.current))

and get_prefix_fn parser = match parser.current with
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

and parse_identifier parser ~identifier =
    parser, Ok (Ast.Identifier identifier)

and parse_int parser ~number =  match int_of_string_opt number with
    | Some value -> parser, Ok (Ast.Integer value)
    | None -> parser, Error (Format.sprintf
        "Unable to convert %s to int"
        number
    )

and parse_float parser ~number = match float_of_string_opt number with
    | Some value -> parser, Ok (Ast.Float value)
    | None -> parser, Error (Format.sprintf
        "Unable to convert %s to float"
        number
    )

and parse_boolean parser ~boolean =
    parser, Ok (Ast.Boolean boolean)

and parse_prefix parser =
    let operator = Option.get parser.current in
    let parser = next_token parser in
    let parser, expr = parse_expression parser `Prefix in
    match expr with 
        | Ok value -> parser, Ok (Ast.Prefix{operator; value})
        | err -> parser, err

and parse_group parser =
    let parser = next_token parser in
    match parser.current with
        | Some Token.Rparen -> 
            next_token parser,
            Ok Ast.Unit
        | _ ->
            let parser, expr = parse_expression parser `Lowest in
            match expr with
                | Ok expr ->
                    begin match parser.peek with
                        | Some Token.Rparen ->
                            next_token parser,
                            Ok expr
                        | Some _ -> parser, Error (peek_error parser Token.Rparen)
                        | None -> parser, Error "No peek token"
                    end
                | err -> parser, err

and parse_block parser =
    let parser = next_token parser in

    let rec parse_block' ?(acc=[]) parser =
        match parser.current with
            | Some Token.Rbrace ->
                next_token parser,
                Ok acc
            | Some Token.Semicolon ->
                let parser = next_token parser in
                parse_block' parser ~acc
            | Some _ ->
                let parser, stmt = parse_statement parser in
                begin match stmt with
                    | Ok stmt -> 
                        let parser = next_token parser in
                        parse_block' parser ~acc:([stmt] @ acc)
                    | Error message -> parser, Error message
                end
            | None -> parser, Error "Missing token"
    in
    match parser.current with
        | Some Token.Rbrace ->
            next_token parser,
            Ok (Ast.Block [Ast.Expression{value= Ast.Unit}])
        | _ ->
            let parser, block = parse_block' parser in
            match block with
                | Ok block -> parser, Ok (Ast.Block (block |> List.rev))
                | Error message -> parser, Error message

and parse_if parser =
    let parser = next_token parser in
    let parser, cond = parse_expression parser `Lowest in
    match cond with
        | Ok cond ->
            begin match parser.peek with
                | Some Token.Lbrace ->
                    let parser = next_token parser in
                    let parser, cons = parse_expression parser `Lowest in
                    begin match cons with
                        | Ok cons -> parse_else parser cond cons
                        | Error message -> parser, Error message
                    end
                | Some _ -> parser, Error (peek_error parser Token.Lbrace)
                | None -> parser, Error "No peek token"
            end
        | err -> parser, err

and parse_else parser cond cons = match parser.current with
    | Some Token.Else ->
        let parser = next_token parser in
        let parser, alt = parse_expression parser `Lowest in
        begin match alt with
            | Ok alt -> parser, Ok (Ast.If
                { condition= cond
                ; consequence= cons
                ; alternative= Some alt;
                }
            )
            | Error message -> parser, Error message
        end
    | _ -> parser, Ok (Ast.If
        { condition= cond
        ; consequence= cons
        ; alternative= None;
        }
    )

and parse_fn_anon parser = match parser.peek with 
    | Some Token.Lbrace ->
        let parser, params = parse_param_list parser in
        begin match params with
            | Ok params ->
                begin match parser.peek with
                    | Some Token.Arrow ->
                        let parser = next_token parser ~count:2 in
                        let parser, expr = parse_expression parser `Lowest in
                        begin match expr with
                            | Ok expr ->
                                next_token parser ~count:2,
                                Ok (Ast.AnonFn
                                    { parameter_list= params
                                    ; block= expr;
                                    })
                            | err -> parser, err
                        end
                    | Some _ -> parser, Error (peek_error parser Token.Arrow)
                    | None -> parser, Error "Missing peek token"
                end
            | Error message -> parser, Error message
        end
    | Some _ -> parser, Error (peek_error parser Token.Lbrace)
    | None -> parser, Error "Missing peek token"

and parse_param_list parser =
    let parser = next_token parser ~count:2 in

    let rec parse_param_list' ?(acc=[]) parser: t * string list =
        match parser.current with
            | Some Token.Ident ident ->
                begin match parser.peek with
                    | Some Token.Comma -> 
                        let parser = next_token parser ~count:2 in
                        parse_param_list' parser ~acc:([ident] @ acc)
                    | _ -> parser, [ident] @ acc
                end
            | _ -> parser, acc
    in
    let parser, params = parse_param_list' parser in
    parser, Ok (params |> List.rev)

and build_infix precedence parser lhs = match parser.peek with
    | x when comp_prec precedence (get_prec x) -> parser, Ok lhs
    | _ ->
        match get_infix_fn parser with
            | Some fn ->
                let parser, expr = fn lhs in
                begin match expr with
                    | Ok expr -> build_infix precedence parser expr
                    | err -> parser, err
                end
            | None -> parser, Ok lhs

and get_infix_fn parser = match parser.peek with
    | Some Token.Plus
    | Some Token.Minus
    | Some Token.Slash
    | Some Token.Asterisk
    | Some Token.Eq
    | Some Token.Neq
    | Some Token.Lt
    | Some Token.Gt
    | Some Token.Leq
    | Some Token.Geq -> Some (parse_infix @@ next_token parser)
    | _ -> None

and parse_infix parser lhs =
    let operator = Option.get parser.current in
    let precedence = get_prec parser.current in
    
    let parser = next_token parser in
    let parser, rhs = parse_expression parser precedence in
    match rhs with
        | Ok rhs -> 
            parser, Ok (Ast.Infix
                { lhs
                ; operator
                ; rhs
                })
        | err -> parser, err
;;

let parse_program parser =
    let rec parse_program' parser stmts errors =
        match parser.current with
            | Some Token.Eof -> parser, stmts, errors
            | _ ->
                let parser, stmt = parse_statement parser in
                let parser = next_token parser in
                match stmt with
                    | Ok stmt -> 
                        parse_program' parser ([stmt] @ stmts) errors
                    | Error message -> 
                        parse_program' parser stmts ([message] @ errors)
    in
    let (parser, statements, errors) = parse_program' parser [] [] in

    let statements = statements |> List.rev in
    let errors = errors |> List.rev in

    parser, Ast.{statements; errors}
;;
