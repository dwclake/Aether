type t = {
    l: Lexer.t,
    errors: list(string),
    cur_t: Token.t,
    peek_t: Token.t
};

let create: Lexer.t => t
let next_token: t => t
let parse_program: t => (t, Ast.program)
