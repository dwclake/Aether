type option_t = Token.t option
[@@deriving show, eq];;

type t = 
    { lexer: Lexer.t ref
    ; current: option_t
    ; peek: option_t;
    }
;;

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

let create lexer =
    { lexer
    ; current= None
    ; peek= None
    }
    |> next_token ~count:2
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
    | Some Token.Lparen -> `Call
    | Some Token.Lbracket -> `Index
    | _ -> `Lowest
;;

let ( let* ) res f = Core.Result.bind res ~f;;

let peek_error parser token =
    Format.sprintf 
        "Expected next token to be %s, got %s instead" 
        (Token.show token) 
        (Token.show (Option.get parser.peek))
;;

let current_error parser token =
    Format.sprintf 
        "Expected current token to be %s, got %s instead" 
        (Token.show token) 
        (Token.show (Option.get parser.current))
;;

let skip_semicolon parser = match parser.peek with
    | Some Token.Semicolon -> next_token parser
    | _ -> parser

let rec parse_statement parser = match parser.current with
    | Some Token.Let -> parse_binding_statement parser Token.Let
    | Some Token.Const -> parse_binding_statement parser Token.Const
    | Some Token.Return -> parse_return_statement parser
    | Some _ -> parse_expression_statement parser
    | None -> Error (parser, "No more tokens")

and parse_binding_statement parser kind = 
    let* parser, name = parse_identifier parser in
    let parser = next_token parser in
    match parser.peek with
        | Some Token.Assign ->
            let parser = next_token parser ~count:2 in
            let* parser, value = parse_expression parser `Lowest in
            Ok (skip_semicolon parser, Ast.Binding
                { kind
                ; name
                ; value
                })
        | _ -> Error (parser, peek_error parser Token.Assign)

and parse_identifier parser = match parser.peek with
    | Some (Token.Ident identifier) -> Ok (parser, {identifier})
    | _ -> Error (parser, "missing identifier.")
    
and parse_return_statement parser =
    let parser = next_token parser in
    let* parser, value = parse_expression parser `Lowest in
    Ok (skip_semicolon parser, Ast.Return{value})

and parse_expression_statement parser =
    let* parser, value = parse_expression parser `Lowest in
    Ok (skip_semicolon parser, Ast.Expression{value})

and parse_expression parser precedence = match get_prefix_fn parser with
    | Some fn ->
        let* parser, lhs = fn parser in
        build_infix precedence parser lhs
    | None -> 
        Error (parser, Format.sprintf
            "No prefix function for %s"
            (Token.to_string @@ Option.get parser.current))

and get_prefix_fn parser = match parser.current with
    | Some (Token.Ident _) -> Some (parse_identifier_expr)
    | Some (Token.Int number) -> Some (parse_int ~number)
    | Some (Token.Float number) -> Some (parse_float ~number)
    | Some Token.True -> Some (parse_boolean ~boolean:true)
    | Some Token.False -> Some (parse_boolean ~boolean:false)
    | Some Token.Bang
    | Some Token.Minus -> Some parse_prefix
    | Some Token.Lparen -> Some parse_group
    | Some Token.Lbrace -> Some parse_block
    | Some Token.If -> Some parse_if
    | Some Token.Pipe -> Some parse_fn_anon
    | _ -> None

and parse_identifier_expr parser = match parser.current with
    | Some (Token.Ident identifier) -> Ok (parser, Ast.Identifier {identifier})
    | _ -> Error (parser, current_error parser (Token.Ident ""))

and parse_int parser ~number =  match int_of_string_opt number with
    | Some value -> Ok (parser, Ast.Integer value)
    | None -> 
        Error (parser, Format.sprintf
            "Unable to convert %s to int"
            number)

and parse_float parser ~number = match float_of_string_opt number with
    | Some value -> Ok (parser, Ast.Float value)
    | None -> 
        Error (parser, Format.sprintf
            "Unable to convert %s to float"
            number)

and parse_boolean parser ~boolean =
    Ok (parser, Ast.Boolean boolean)

and parse_prefix parser =
    let operator = Option.get parser.current in
    let parser = next_token parser in
    let* parser, value = parse_expression parser `Prefix in
    Ok (parser, Ast.Prefix{operator; value})

and parse_group parser =
    let parser = next_token parser in
    match parser.current with
        | Some Token.Rparen -> Ok (parser, Ast.Unit)
        | _ ->
            let* parser, expr = parse_expression parser `Lowest in
            match parser.peek with
                | Some Token.Rparen -> Ok (next_token parser, expr)
                | Some _ -> Error (parser, peek_error parser Token.Rparen)
                | None -> Error (parser, "No peek token")

and parse_block parser =
    let rec parse_block' ?(acc=[]) parser =
        match parser.current with
            | Some Token.Rbrace -> Ok (parser, acc)
            | Some Token.Semicolon ->
                let parser = next_token parser in
                parse_block' parser ~acc
            | Some _ ->
                let* parser, stmt = parse_statement parser in
                let parser = next_token parser in
                parse_block' parser ~acc:([stmt] @ acc)
            | None -> Error (parser, "Missing token")
    in
    let parser = next_token parser in
    match parser.current with
        | Some Token.Rbrace -> 
            Ok (next_token parser, Ast.Block 
                [Ast.Expression{value= Ast.Unit}])
        | _ ->
            let* parser, block = parse_block' parser in
            Ok (parser, Ast.Block (block |> List.rev))

and parse_if parser =
    let parser = next_token parser in
    let* parser, cond = parse_expression parser `Lowest in
    begin match parser.peek with
        | Some Token.Lbrace ->
            let parser = next_token parser in
            let* parser, cons = parse_expression parser `Lowest in
            parse_else parser cond cons
        | Some _ -> Error (parser, peek_error parser Token.Lbrace)
        | None -> Error (parser, "No peek token")
    end

and parse_else parser cond cons = 
    let parser = next_token parser in
    match parser.current with
    | Some Token.Else ->
        let parser = next_token parser in
        let* parser, alt = parse_expression parser `Lowest in
        Ok (parser, Ast.If
            { condition= cond
            ; consequence= cons
            ; alternative= Some alt;
            }
        )
    | _ -> 
        Ok (parser, Ast.If
            { condition= cond
            ; consequence= cons
            ; alternative= None;
            }
        )

and parse_fn_anon parser =
    let* parser, params = parse_param_list parser in
    match parser.current with
        | Some Token.FatArrow ->
            let parser = next_token parser in
            let* parser, expr = parse_expression parser `Lowest in
            Ok (parser, Ast.AnonFn
                { parameters= params
                ; block= expr
                ; arity= List.length params
                })
        | Some _ -> Error (parser, current_error parser Token.FatArrow)
        | None -> Error (parser, "Missing peek token")

and parse_param_list parser =
    let rec parse_param_list' ?(acc=[]) parser =
        let* parser, ident = parse_identifier parser in
        let parser = next_token parser in
        match parser.peek with
            | Some Token.Comma -> 
                let parser = next_token parser in
                parse_param_list' parser ~acc:([ident] @ acc)
            | _ -> Ok (parser, [ident] @ acc)
    in
    let* parser, params = parse_param_list' parser in
    let parser = next_token parser ~count:2 in
    Ok (parser, params |> List.rev)

and build_infix precedence parser lhs = match parser.peek with
    | x when comp_prec precedence (get_prec x) -> Ok (parser, lhs)
    | _ ->
        match get_infix_fn parser with
            | Some fn ->
                let* parser, expr = fn parser lhs in
                build_infix precedence parser expr
            | None -> Ok (parser, lhs)

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
    | Some Token.Geq -> Some (parse_infix)
    | Some Token.Lparen -> Some (parse_call)
    | _ -> None

and parse_infix parser lhs =
    let parser = next_token parser in
    let operator = Option.get parser.current in
    let precedence = get_prec parser.current in
    
    let parser = next_token parser in
    let* parser, rhs = parse_expression parser precedence in
    Ok (parser, Ast.Infix
        { lhs
        ; operator
        ; rhs
        })

and parse_call parser expr =
    match expr with
        | Ast.Identifier _ as ident->
            let* parser, args = parse_arguments parser in
            Ok (next_token parser, Ast.FnCall
                { fn= ident
                ; arguments= args
                })
        | _ -> Error (parser, "Fn call missing identifier.")

and parse_arguments parser =
    let rec parse_arguments' ?(acc=[]) parser =
        let* parser, expr = parse_expression parser `Lowest in
        match parser.peek with
            | Some Token.Comma -> 
                let parser = next_token parser ~count:2 in
                parse_arguments' parser ~acc:([expr] @ acc)
            | _ -> Ok (parser, [expr] @ acc)
    in
    let parser = next_token parser ~count:2 in
    match parser.current with
        | Some Token.Rparen -> Ok(parser, [Ast.Unit])
        | _ ->
            let* parser, args = parse_arguments' parser in
            let parser = next_token parser in
            Ok (parser, args |> List.rev)
;;

let parse_program parser =
    let rec parse_program' parser stmts =
        match parser.current with
            | Some Token.Eof -> Ok (parser, stmts)
            | _ ->
                let* parser, stmt = parse_statement parser in
                let parser = next_token parser in
                parse_program' parser ([stmt] @ stmts)
    in
    let* parser, statements = parse_program' parser [] in
    let statements = statements |> List.rev in

    Ok (parser, Ast.{statements})
;;
