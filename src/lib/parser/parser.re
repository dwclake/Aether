[@deriving (show, eq)]
type option_t = option(Token.t);

type t = {
    lexer: Lexer.t,
    current: option_t,
    peek: option_t,
};

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


[@deriving (ord)]
type precedence = [ 
    | `Lowest
    | `Equals
    | `LessGreater
    | `Sum
    | `Product
    | `Prefix
    | `Call
    | `Index
];

let comp_prec(a, b) = {compare_precedence(a, b) >= 0};

let get_prec(token): precedence = {
    switch token {
        | Some(Token.EqualTo)
        | Some(Token.NotEq) => `Equals
        | Some(Token.Lesser)
        | Some(Token.LesserEq)
        | Some(Token.Greater)
        | Some(Token.GreaterEq) => `LessGreater
        | Some(Token.Plus)
        | Some(Token.Minus) => `Sum
        | Some(Token.Forwardslash)
        | Some(Token.Asterisk) => `Product
        | _ => `Lowest
    }
};

let rec parse_statement(parser: t) = {
    switch parser.current {
        | Some(Token.Let) => parse_bind_statement(parser)
        | Some(Token.Const) => parse_bind_statement(parser)
        | Some(Token.Return) => parse_return_statement(parser)
        | _ => parse_expression_statement(parser)
    }
}

and parse_bind_statement(parser: t) = {
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
                            let binding = Ast.Binding{
                                kind: current,
                                name,
                                value
                            };

                            (parser, Ok(binding))
                        }
                        | Error(message) => (parser, Error(message))
                    }
                }
                | _ => (parser, Error(peek_error(parser, Token.Assign)))
            }
        } 
        | _ => (parser, Error(peek_error(parser, Token.Ident("\000"))))
    }
}

and parse_return_statement(parser: t) = {
    let (parser, expr) = parse_expression(next_token(parser), `Lowest);

    switch expr {
        | Ok(value) => {
            (parser, Ok(Ast.Return{value: value}))
        }
        | Error(message) => {
            (parser, Error(message))
        }
    }
}

and parse_expression_statement(parser: t) = {
    let (parser, expr) = parse_expression(parser, `Lowest);
    let parser = if(parser.peek == Some(Token.Semicolon)) {
        next_token(parser)
    } else {
        parser
    };

    switch expr {
        | Ok(value) => (parser, Ok(Ast.Expression{value: value}))
        | Error(message) => (parser, Error(message))
    }
}

and parse_expression(parser: t, precedence: precedence) = {
    switch (get_prefix_fn(parser)) {
        | Some(fn) => {
            let (parser, prefix) = fn(parser);
            switch prefix {
                | Ok(lhs) => build_infix(precedence, parser, lhs)
                | err => (parser, err)
            }
        }
        | None => (parser, Error(Format.sprintf(
            "No prefix function for %s",
            Token.to_string(Option.get(parser.current))
        )))
    }
}

and get_prefix_fn(parser: t) = {
    switch parser.current {
        | Some(Token.Bang)
        | Some(Token.Minus) => Some(parse_prefix)
        | Some(Token.True)
        | Some(Token.False) => Some(parse_boolean)
        | Some(Token.Lparen) => Some(parse_group)
        | Some(Token.Ident(identifier)) => Some(parse_identifier(~identifier))
        | Some(Token.Int(number)) => Some(parse_int(~number))
        | Some(Token.Float(number)) => Some(parse_float(~number))
        | Some(Token.If) => Some(parse_if)
        | _ => None
    }
}

and parse_boolean(parser: t) = {
    let boolean = bool_of_string_opt(Token.to_string_opt(parser.current));
    switch boolean {
        | Some(boolean) => (parser, Ok(Ast.Boolean(boolean)))
        | None => (parser, Error(Format.sprintf(
            "Could not convert %s to boolean",
            Token.to_string_opt(parser.current)
        )))
    }
}

and parse_identifier(parser: t, ~identifier: string) = {
    let id = Ast.Identifier(identifier);
    (parser, Ok(id))
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

and parse_group(parser: t) = {
    let parser = next_token(parser);
    let (parser, expr) = parse_expression(parser, `Lowest);

    switch expr {
        | Ok(expr) => {
            switch parser.peek {
                | Some(token) => {
                    switch token {
                        | Token.Rparen => (next_token(parser), Ok(expr))
                        | _ as token => (parser, Error(Format.sprintf(
                            "Expected ) got %s",
                            Token.to_string(token)
                        )))
                    }
                }
                | None => (parser, Error("no peek token"))
            }
        }
        | err => (parser, err)
    }
}

and parse_if(parser: t) = {
    let parser = next_token(parser);
    let (parser, cond) = parse_expression(parser, `Lowest);
    switch cond {
        | Ok(cond) => {
            switch parser.peek {
                | Some(token) => {
                    switch token {
                        | Token.Lsquirly => {
                            let (parser, cons) = parse_block_statement(parser);
                            switch cons {
                                | Ok(cons) => {
                                    switch parser.current {
                                        | Some(Token.Else) => {
                                            let (parser, alt) = parse_block_statement(parser);
                                            switch alt {
                                                | Ok(alt) => (parser, Ok(Ast.If{
                                                    condition: cond,
                                                    consequence: cons,
                                                    alternative: Some(alt)
                                                }))
                                                | Error(message) => (parser, Error(message))
                                            }
                                        }
                                        | _ => (parser, Ok(Ast.If{
                                            condition: cond,
                                            consequence: cons,
                                            alternative: None
                                        }))
                                    }
                                }
                                | Error(message) => (parser, Error(message))
                            }
                        }
                        | _ as token => (parser, Error(Format.sprintf(
                            "Expected ) got %s",
                            Token.to_string(token)
                        )))
                    }
                }
                | None => (parser, Error("no peek token"))
            }           
        }
        | err => (parser, err)
    }
}

and parse_block_statement(parser: t) = {
    let parser = parser
        |> next_token
        |> next_token;

    let rec parse_block_statement'(~acc=[], parser: t) = {
        switch parser.current {
            | Some(Token.Rsquirly)
            | Some(Token.Eof) => (next_token(parser), Ok(acc))
            | Some(_) => {
                let (parser, stmt) = parse_statement(parser);
                switch stmt {
                    | Ok(stmt) => parse_block_statement'(next_token(parser), ~acc=[stmt] @ acc)
                    | Error(message) => (parser, Error(message))
                }
            }
            | None => (parser, Error("Missing token"))
        }
    }

    let (parser, block) = parse_block_statement'(parser);
    switch block {
        | Ok(block) => (parser, Ok(block |> List.rev))
        | err => (parser, err)
    }
}

and parse_prefix(parser: t) = {
    let operator = Option.get(parser.current);
    let parser = next_token(parser);
    let (parser, expr) = parse_expression(parser, `Prefix);
    switch expr {
        | Ok(value) => (parser, Ok(Ast.Prefix{operator, value}))
        | err => (parser, err)
    }
}

and build_infix(precedence, parser, lhs) = {
    switch parser.peek {
        | x when comp_prec(precedence, get_prec(x)) => (parser, Ok(lhs))
        | _ => {
            switch (get_infix_fn(parser)) {
                | Some(fn) => {
                    let (parser, expr) = fn(lhs);
                    switch expr {
                        | Ok(expr) => {
                            build_infix(precedence, parser, expr);
                        }
                        | err => (parser, err)
                    };
                }
                | None => (parser, Ok(lhs))
            }
        }
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
        | Some(GreaterEq) => Some(parse_infix(next_token(parser)))
        | _ => None
    }    
}

and parse_infix(parser: t, lhs: Ast.expression) = {
    let operator = Option.get(parser.current);
    let precedence = get_prec(parser.current);

    let (parser, rhs) = parse_expression(next_token(parser), precedence);
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
};


let parse_program(parser: t): (t, Ast.program) = { 
    let rec parse_program' = (parser: t, stmts, errors) => {
        switch parser.current {
            | Some(Token.Eof) => (parser, stmts, errors)
            | _ => {
                let (parser, stmt) = parse_statement(parser);
                switch stmt {
                    | Ok(stmt) => parse_program'(next_token(parser), [stmt] @ stmts, errors)
                    | Error(message) => parse_program'(next_token(parser), stmts, [message] @ errors)
                }
            }
        }
    };
    let (parser, statements, errors) = parse_program'(parser, [], []);

    let errors = errors |> List.rev;
    let statements = statements |> List.rev;
    
    (parser, {statements, errors})
};
