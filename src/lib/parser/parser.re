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
        Token.show(Option.get(parser.peek))
    )
};

let rec parse_expression(parser: t, ~infix=false, _: _precedence) = {
    switch infix {
        | true => {
            switch (get_prefix_fn(parser)) {
                | Some(fn) => fn(parser)
                | None => {
                    switch parser.current {
                        | Some(token) => {
                            switch token {
                                | Token.Ident(identifier) => {
                                    let id = Ast.Identifier(identifier);

                                    (parser, Ok(id))
                                }
                                | Token.Int(value) => parse_int(parser, ~number=value)
                                | Token.Float(value) => parse_float(parser, ~number=value)
                                | _ => (parser, Error("Cannot parse expression"))
                            }
                        }
                        | None => (parser, Error("Missing token"))
                    }
                }
            }
        }
        | false => {
            switch (get_infix_fn(parser)) {
                | Some(fn) => fn(parser)
                | None => {
                    switch (get_prefix_fn(parser)) {
                        | Some(fn) => fn(parser)
                        | None => {
                            switch parser.current {
                                | Some(token) => {
                                    switch token {
                                        | Token.Ident(identifier) => {
                                            let id = Ast.Identifier(identifier);

                                            (parser, Ok(id))
                                        }
                                        | Token.Int(value) => parse_int(parser, ~number=value)
                                        | Token.Float(value) => parse_float(parser, ~number=value)
                                        | _ => (parser, Error("Cannot parse expression"))
                                    }
                                }
                                | None => (parser, Error("Missing token"))
                            }
                        }
                    }
                }
            }
        }
    }
}

and parse_int(parser: t, ~number: string) = {
    switch (int_of_string_opt(number)) {
        | Some(value) => (parser, Ok(Ast.Integer(value)))
        | None => (parser, Error(Format.sprintf(
            "Unable to convert %s to float", 
            number
        )))
    }
}

and parse_float(parser: t, ~number: string) = {
    switch (float_of_string_opt(number)) {
        | Some(value) => (parser, Ok(Ast.Float(value)))
        | None => (parser, Error(Format.sprintf(
            "Unable to convert %s to float", 
            number
        )))
    }
}

and parse_prefix_negation(parser: t) = {
    let parser = next_token(parser);
    let (parser, expr) = parse_expression(parser, `Lowest);
    switch expr {
        | Ok(value) => (parser, Ok(Ast.Prefix{operator: Token.Bang, value}))
        | err => (parser, err)
    }
}

and parse_prefix_negative(parser: t) = {
    let parser = next_token(parser);
    let (parser, expr) = parse_expression(parser, `Lowest);
    switch expr {
        | Ok(value) => (parser, Ok(Ast.Prefix{operator: Token.Minus, value}))
        | err => (parser, err)
    }
}

and get_prefix_fn(parser: t) = {
    switch parser.current {
        | Some(Token.Bang) => Some(parse_prefix_negation)
        | Some(Token.Minus) => Some(parse_prefix_negative)
        | _ => None
    }
}

and infix_fn(parser: t) = {
    let operator = Option.get(parser.peek);

    let (parser, lhs) = parse_expression(parser, `Lowest, ~infix=true);
    switch lhs {
        | Ok(lhs) => {
            let parser = parser |> next_token |> next_token;
            let (parser, rhs) = parse_expression(parser, `Lowest);
            switch rhs {
                | Ok(rhs) => {
                    let expression = Ast.Infix{
                        lhs,
                        operator,
                        rhs
                    };
                    (parser, Ok(expression))
                }
                | err => (parser, err)
            }
        }
        | err => (parser, err)
    }
}

and get_infix_fn(parser: t) = {
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
        | Some(GreaterEq) => Some(infix_fn)
        | Some(Lparen) => Some(infix_fn)
        | Some(Lbracket) => Some(infix_fn)
        | _ => None
    }    
};

let parse_bind_statement(parser: t) = {
    let current = Option.get(parser.current);
    switch parser.peek {
        | Some(Token.Ident(name)) => {
            let parser = next_token(parser);
            switch parser.peek {
                | Some(Token.Assign) => {
                    let parser = parser |> next_token |> next_token;
                    let (parser, expr) = parse_expression(parser, `Lowest);

                    switch expr {
                        | Ok(value) => {
                            let binding = switch current {
                                | Let => Some(Ast.Let{name, value})
                                | Const => Some(Ast.Const{name, value})
                                | _ => None
                            };
                            let binding = switch binding {
                                | Some(expr) => Ok(expr)
                                | None => Error("Token not associated with bindings")
                            };

                            (parser, binding)
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
    let parser = if(parser.peek == Some(Token.Semicolon)) {
        next_token(parser)
    } else {
        parser
    };

    switch expr {
        | Ok(value) => (parser, Ok(Ast.ExpressionStatement{value: value}))
        | Error(message) => (parser, Error(message))
    }
}

let parse_statement(parser: t) = {
    switch parser.current {
        | Some(Token.Let) => parse_bind_statement(parser)
        | Some(Token.Const) => parse_bind_statement(parser)
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
