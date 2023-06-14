type t = {
    l: Lexer.t,
    errors: list(string),
    cur_t: Token.t,
    peek_t: Token.t
};

type par_r = {
    p: t, 
    stmt: option(Ast.statement)
};

let next_token = (p: t): t => {
    let lex = Lexer.next_token(p.l);
    
    {
        ...p,
        l: lex#l,
        cur_t: p.peek_t,
        peek_t: lex#t,
    }
};

let create = (l: Lexer.t): t => {
    {
        l,
        errors: [],
        cur_t: Token.EOF,
        peek_t: Token.EOF
    } 
    |> next_token |> next_token
};

let peek_error = (p: t, t: Token.t): t => {
    let error = Format.sprintf(
        "Expected next token to be %s, got %s instead",
        Token.show(t),
        Token.show(p.peek_t)
    );

    {...p, errors: p.errors @ [error]}
};

let parse_let_statement = (p: t): par_r => {
    switch p.peek_t {
        | IDENT(s) => {
            open Ast;

            let p = next_token(p);
            let name = {identifier: s};

            switch p.peek_t {
                | ASSIGN => {
                    // expressions will be parsed here later
                    let rec loop = (p: t) => {
                        switch p.cur_t {
                            | SEMICOLON => p
                            | _ => loop(next_token(p))
                        }
                    };

                    let p = loop(p);
                    {p, stmt: Some(LET{name, value: IDENTIFIER(name)})}
                }
                | _ => {
                    let p = peek_error(p, Token.ASSIGN);
                    {p, stmt: None}
                }
            }
        } 
        | _ => {
            let p = peek_error(p, Token.IDENT(""));
            {p, stmt: None}
        }
    }
};

let parse_statement = (p: t): par_r => {
    switch p.cur_t {
        | Token.LET => parse_let_statement(p)
        | _ => {p, stmt: None}
    }
};

let parse_program = (p: t): (t ,Ast.program) => { 
    let rec loop = (p: t, stmts) => {
        switch p.cur_t {
            | Token.EOF => (p, stmts)
            | _ => {
                let par = parse_statement(p);
                switch par.stmt {
                    | Some(s) => loop(next_token(par.p), stmts @ [s])
                    | None => loop(next_token(par.p), stmts)
                }
            }
        }
    }

    let (p, statements) = loop(p, []);
    
    (p, {statements: statements})
};
