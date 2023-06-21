[@deriving (show, eq)]
type option_t = option(Token.t);

type t = {
    lexer: Lexer.t,
    current: option_t,
    peek: option_t,
};

//[@deriving (show, ord)]
type _precedence = [ 
    | `Lowest
    | `Equals
    | `LessGreater
    | `Sum
    | `Product
    | `Prefix
    | `Call
    | `Index
];

type _binding = [
    | `Let
    | `Const
];

type _check_token = [ 
    | `Current
    | `Peek
];

let next_token(parser: t): t = {
    let lex = Lexer.next_token(parser.lexer);
    
    {   lexer: lex#lexer,
        current: parser.peek,
        peek: Some(lex#token),
    }
};

let create(lexer: Lexer.t): t = {
    {   lexer,
        current: None,
        peek: None,
    } 
    |> next_token |> next_token
};

let peek_error(parser: t, token: Token.t): string = {
    Format.sprintf(
        "Expected next token to be %s, got=%s",
        Token.show(token),
        Token.show(parser.peek |> Option.get)
    )
};

let _get_infix_fn(parser: t) = {
    switch parser.peek {
        | Some(Plus)
        | Some(Minus)
        | Some(Forwardslash)
        | Some(Asterisk)
        | Some(EqualTo)
        | Some(NotEq)
        | Some(Lesser)
        | Some(Greater)
        | Some(LesserEq)
        | Some(GreaterEq) => Some(())
        | Some(Lparen) => Some(())
        | Some(Lbracket) => Some(())
        | _ => None
    }    
};

let parse_expression(parser: t, ~check=`Current,  _: _precedence) = {
    let checked_token = switch check {
        | `Current => parser.current
        | `Peek => parser.peek
    };
    let parser = switch check {
        | `Current => parser
        | `Peek => next_token(parser)
    };

    switch checked_token {
        | Some(token) => {
            switch token {
                | Token.Ident(identifier) => {
                    let id = Ast.Identifier(identifier);
                    (parser, Ok(id))
                }
                | Token.Int(value) => {
                    switch (int_of_string_opt(value)) {
                        | Some(value) => (parser, Ok(Ast.Integer(value)))
                        | None => (parser, Error(Format.sprintf(
                            "Unable to convert %s to int", 
                            value
                        )))
                    }
                }
                | Token.Float(value) => {
                    switch (float_of_string_opt(value)) {
                        | Some(value) => (parser, Ok(Ast.Float(value)))
                        | None => (parser, Error(Format.sprintf(
                            "Unable to convert %s to float", 
                            value
                        )))
                    }
                }
                | _ => (parser, Error("Not a expression"))
            }
        }
        | None => (parser, Error("Missing token"))
    }
};

let parse_bind_statement(~bind=`Let, parser: t) = {
    switch parser.peek {
        | Some(Token.Ident(identifier)) => {
            let parser = next_token(parser);
            let name: Ast.identifier = {identifier};

            switch parser.peek {
                | Some(Token.Assign) => {
                    let parser = next_token(parser);
                    let (parser, expr) = parse_expression(parser, `Lowest, ~check=`Peek);

                    switch expr {
                        | Ok(value) => {
                            let lexer = switch bind {
                                | `Let => Ast.Let{name, value}
                                | `Const => Ast.Const{name, value}
                            };
                            (parser, Ok(lexer))
                        }
                        | Error(message) => (parser, Error(message))
                    }
                }
                | _ => {
                    let message = peek_error(parser, Token.Assign);
                    (parser, Error(message))
                }
            }
        } 
        | _ => {
            let message = peek_error(parser, Token.Ident("\000"));
            (parser, Error(message))
        }
    }
};

let parse_return_statement(parser: t) = {
    let parser = next_token(parser);
    let (parser, expr) = parse_expression(parser, `Lowest);

    switch expr {
        | Ok(value) => {
            (parser, Ok(Ast.Return{value: value}))
        }
        | Error(message) => {
            (parser, Error(message))
        }
    }
};

let parse_expression_statement(parser: t) = {
    let (parser, expr) = parse_expression(parser, `Lowest);

    let parser = if(parser.peek == Some(Token.Semicolon)) {next_token(parser)} else {parser};

    switch expr {
        | Ok(value) => (parser, Ok(Ast.ExpressionStatement{value: value}))
        | Error(message) => (parser, Error(message))
    }
}

let parse_statement(parser: t) = {
    switch parser.current {
        | Some(Token.Let) => parse_bind_statement(parser, ~bind=`Let)
        | Some(Token.Const) => parse_bind_statement(parser, ~bind=`Const)
        | Some(Token.Return) => parse_return_statement(parser)
        | _ => parse_expression_statement(parser)
    }
};

let parse_program(parser: t): (t, Ast.program) = { 
    let rec loop = (parser: t, stmts, errors) => {
        switch parser.current {
            | Some(Token.Eof) => (parser, stmts, errors)
            | _ => {
                let (parser, stmt) = parse_statement(parser);
                switch stmt {
                    | Ok(stmt) => loop(next_token(parser), [stmt] @ stmts, errors)
                    | Error(message) => loop(next_token(parser), stmts, [message] @ errors)
                }
            }
        }
    };
    let (parser, statements, errors) = loop(parser, [], []);

    let errors = errors |> List.rev;
    let statements = statements |> List.rev;
    
    (parser, {statements, errors})
};
