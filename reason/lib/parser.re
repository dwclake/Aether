type t = {
    l: Lexer.t,

    cur_t: Token.t,
    peek_t: Token.t
};

type par_r = {. p: t, stmt: result(Ast.statement, string)};

let (++) = (p, stmt): par_r => {
    { as _;
        pub p = p;
        pub stmt = stmt;
    }
};

let next_token = (p: t): t => {
    let lex = Lexer.next_token(p.l);
    {
        l: lex#l,
        cur_t: p.peek_t,
        peek_t: lex#t
    }
};

let create = (l: Lexer.t): t => {
    {
        l,
        cur_t: Token.EOF,
        peek_t: Token.EOF
    } |> next_token |> next_token
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
                    (++) (p, Ok(LET{
                        name,
                        value: IDENTIFIER(name)
                    }))
                }
                | _ => (++) (p, Error("identifier must be followed by an ="))
            }
        } 
        | _ => (++) (p, Error("let must be followed by an identifier"))
    }
};

let parse_statement = (p: t): par_r => {
    switch p.cur_t {
        | Token.LET => parse_let_statement(p)
        | _ => (++) (p, Error("Only supports let statements currently"))
    }
};

let parse_program = (p: t): result(Ast.program, string) => { 
    let rec loop = (p: t, stmts) => {
        switch p.cur_t {
            | Token.EOF => stmts
            | _ => {
                let par = parse_statement(p);
                switch par#stmt {
                    | Ok(s) => loop(next_token(par#p), stmts @ [s])
                    | Error(e) => {
                        Stdio.eprintf("%s\n", e);
                        loop(next_token(par#p), stmts)
                    }
                }
            }
        }
    }

    let statements = loop(p, []);
    switch statements {
        | [] => Error("No statements in program")
        | _ => Ok({
            statements: statements
        })
    }
};
