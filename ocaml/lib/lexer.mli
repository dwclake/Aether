type t

class ['a] lex_r:
    t -> 'a ->
    object
        method l: t
        method content: 'a
    end

val create: input: string -> t
(* Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer *)
(* with the char after that token stored in it *)
val next_token: t -> Token.t lex_r
