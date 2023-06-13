type t = {
    l: Lexer.t,

    cur_t: Token.t,
    peek_t: Token.t
};

type par_r = {. 
    p: t, 
    stmt: result(Ast.statement, string)
};

let create: Lexer.t => t
let next_token: t => t
let parse_program: t => result(Ast.program, string)
