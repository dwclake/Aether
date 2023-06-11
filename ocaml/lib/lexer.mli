type t

class ['a] lex_r:
    t -> 'a ->
    object
        method l: t
        method value: 'a
    end

val create: input: string -> t
(* Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer *)
val next_token: t -> Token.t lex_r
