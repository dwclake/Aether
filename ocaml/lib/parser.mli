type t = {
    l: Lexer.t;

    cur_t: Token.t;
    peek_t: Token.t;
}

type par_r = <
    p: t;
    stmt: (Ast.statement, string) result;
>

val create: Lexer.t -> t
val next_token: t -> t
val parse_program: t -> (Ast.program, string) result
