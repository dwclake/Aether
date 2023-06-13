type t = {
    l: Lexer.t;

    cur_t: Token.t;
    peek_t: Token.t;
}

class par_r:
    t -> (Ast.statement, string) result ->
    object
        method p: t
        method stmt: (Ast.statement, string) result
    end

val create: Lexer.t -> t
val next_token: t -> t
val parse_program: t -> (Ast.program, string) result
