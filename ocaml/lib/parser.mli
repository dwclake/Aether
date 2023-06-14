type t = {
    l: Lexer.t;
    errors: string list;
    cur_t: Token.t;
    peek_t: Token.t;
}

type par_r = <
    p: t;
    stmt: Ast.statement option;
>

val create: Lexer.t -> t
val next_token: t -> t
val parse_program: t -> (t * Ast.program)
