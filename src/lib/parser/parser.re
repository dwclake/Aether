type t = {
    l: Lexer.t,
    errors: list(string),
    cur_t: Token.t,
    peek_t: Token.t
};

type par_r = {
    p: t, 
    node: option(Ast.node)
};

let next_token(p: t): t = {
    let lex = Lexer.next_token(p.l);
    
    {   ...p,
        l: lex#l,
        cur_t: p.peek_t,
        peek_t: lex#t,
    }
};

let create(l: Lexer.t): t = {
    {   l,
        errors: [],
        cur_t: Token.Eof,
        peek_t: Token.Eof
    } 
    |> next_token |> next_token
};

let peek_error(p: t, t: Token.t): t = {
    let error = Format.sprintf(
        "Expected next token to be %s, got %s instead",
        Token.show(t),
        Token.show(p.peek_t)
    );

    {...p, errors: p.errors @ [error]}
};

let _expect_token(p: t, t: Token.t) = { 
    let a = Obj.repr(p.peek_t);
    let b = Obj.repr(t);

    let res = switch (Obj.is_block(a), Obj.is_block(b)) {
        | (true, true) => Obj.tag(a) == Obj.tag(b)
        | (false, false) => a == b
        | _ => false
    };

    if (res) {
        let p = next_token(p);
        (p, true)
    } else {
        let p = peek_error(p, t);
        (p, false)
    }
};

let parse_let_statement(p: t): par_r = {
    switch p.peek_t {
        | Token.Ident(s) => {
            open Ast;

            let p = next_token(p);
            let name = {identifier: s};

            switch p.peek_t {
                | Token.Assign => {
                    // expressions will be parsed here later
                    let rec skip = (p: t) => {
                        switch p.cur_t {
                            | Token.Semicolon => p
                            | _ => skip(next_token(p))
                        }
                    };

                    let p = skip(p);
                    let l = Ast.Let{name, value: Ast.Identifier(name)};
                    
                    {p, node: Some(Ast.Statement(l))}
                }
                | _ => {
                    let p = peek_error(p, Token.Assign);
                    {p, node: None}
                }
            }
        } 
        | _ => {
            let p = peek_error(p, Token.Ident(""));
            {p, node: None}
        }
    }
};

let parse_return_statement(p: t): par_r = {
    let p = next_token(p);

    let rec skip = (p: t) => {
        switch p.cur_t {
            | Token.Semicolon => p
            | _ => skip(next_token(p))
        }
    };

    let p = skip(p);
    let i = Ast.Identifier{identifier: ""};
    let r = Ast.Return{value: i};

    {p, node: Some(Ast.Statement(r))}
};

let parse_statement(p: t): par_r = {
    switch p.cur_t {
        | Token.Let => parse_let_statement(p)
        | Token.Return => parse_return_statement(p)
        | _ => {p, node: None}
    }
};

let parse_program(p: t): (t, Ast.program) = { 
    let rec loop = (p: t, stmts) => {
        switch p.cur_t {
            | Token.Eof => (p, stmts)
            | _ => {
                let par = parse_statement(p);
                switch par.node {
                    | Some(s) => loop(next_token(par.p), [s] @ stmts)
                    | None => loop(next_token(par.p), stmts)
                }
            }
        }
    };

    let (p, statements) = loop(p, []);
    let statements = statements |> List.rev;
    
    (p, {statements: statements})
};
