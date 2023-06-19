[@deriving (show, eq)]
type option_t = option(Token.t);

type t = {
    l: Lexer.t,
    current: option_t,
    peek: option_t,
};

[@deriving (show, ord)]
type precedence = 
    [ `Lowest
    | `Equals
    | `LessGreater
    | `Sum
    | `Product
    | `Prefix
    | `Call
    | `Index
];

let next_token(p: t): t = {
    let lex = Lexer.next_token(p.l);
    
    {   l: lex#l,
        current: p.peek,
        peek: Some(lex#t),
    }
};

let create(l: Lexer.t): t = {
    {   l,
        current: None,
        peek: None,
    } 
    |> next_token |> next_token
};

let peek_error(p: t, t: Token.t) = {
    Format.sprintf(
        "Expected next token to be %s, got=%s",
        Token.show(t),
        Token.show(p.peek |> Option.get)
    )
};

let _get_infix_fn(p: t) = {
    switch p.peek {
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

let parse_expression(p: t, _: precedence) = {
    switch p.current {
        | Some(t) => {
            switch t {
                | Token.Ident(i) => {
                    let id = Ast.Identifier{identifier: i};
                    let p = next_token(p);
                    (p, Ok(id))
                }
                | _ => (next_token(p), Error("Not a expression"))
            }
        }
        | None => (next_token(p), Error("Missing token"))
    }
};

let parse_let_statement(p: t) = {
    switch p.peek {
        | Some(Token.Ident(s)) => {
            open Ast;

            let p = next_token(p);
            let name = {identifier: s};

            switch p.peek {
                | Some(Token.Assign) => {
                    let (p, value) = parse_expression(p, `Lowest);

                    switch value {
                        | Ok(v) => {
                            let l = Ast.Let{name, value: v};

                            (p, Ok(l))
                        }
                        | Error(e) => (p, Error(e))
                    }
                }
                | _ => {
                    let message = peek_error(p, Token.Assign);
                    (p, Error(message))
                }
            }
        } 
        | _ => {
            let message = peek_error(p, Token.Ident(""));
            (p, Error(message))
        }
    }
};

let parse_return_statement(p: t) = {
    let p = next_token(p);

    let (p, value) = parse_expression(p, `Lowest);

    switch value {
        | Ok(v) => {
            (p, Ok(Ast.Return{value: v}))
        }
        | Error(_) => {
            let i = Ast.Identifier{identifier: ""};

            (p, Ok(Ast.Return{value: i}))
            //(p, Error(e))
        }
    }
};

let parse_expression_statement(p: t) = {
    let (p, expr) = parse_expression(p, `Lowest);

    let p = if (p.peek == Some(Token.Semicolon)) {
        next_token(p)
    } else {
        p
    }

    switch expr {
        | Ok(ex) => (p, Ok(Ast.ExpressionStatement{value: ex}))
        | Error(e) => (p, Error(e))
    }
}

let parse_statement(p: t) = {
    switch p.current {
        | Some(Token.Let) => parse_let_statement(p)
        | Some(Token.Return) => parse_return_statement(p)
        | _ => parse_expression_statement(p)
    }
};

let parse_program(p: t): (t, Ast.program) = { 
    let rec loop = (p: t, stmts, errs) => {
        switch p.current {
            | Some(Token.Eof) => (p, stmts, errs)
            | _ => {
                let (par, stmt) = parse_statement(p);
                switch stmt {
                    | Ok(s) => loop(next_token(par), [s] @ stmts, errs)
                    | Error(e) => loop(next_token(par), stmts, [e] @ errs)
                }
            }
        }
    };
    let (p, statements, errors) = loop(p, [], []);

    let errors = errors |> List.rev;
    let statements = statements |> List.rev;
    
    (p, {statements, errors})
};
