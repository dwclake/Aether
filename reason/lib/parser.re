type t = {
    l: Lexer.t,

    cur_t: Token.t,
    peek_t: Token.t
};

type par_r = {. p: t, stmt: result(Ast.statement, string)};

let (++) = (parser: t, stmt: result(Ast.statement, string)) => {
    { as _;
        pub p = parser;
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
    } 
    |> next_token 
    |> next_token
};

let parse_let_statement = (p: t): par_r => {
    p ++ Error("Unimplemented")
};

let parse_statement = (p: t): par_r => {
    switch p.cur_t {
        | Token.LET => parse_let_statement(p)
        | _ => (++) (p, Error("Only supports let statements currently"))
    }
};

let parse_program = (p: t): result(Ast.program, string) => { 
    let rec loop = (p: t, stmts: list(Ast.statement)) => {
        switch p.cur_t {
            | Token.EOF => stmts
            | _ => {
                let par = parse_statement(p);
                switch par#stmt {
                    | Ok(s) => loop(par#p |> next_token, stmts @ [s])
                    | Error(e) => Stdio.eprintf("%s\n", e);
                                  loop(par#p |> next_token, stmts)
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
