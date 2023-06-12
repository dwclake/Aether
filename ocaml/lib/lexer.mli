type t

(* class ['a] lex_r:
    t -> 'a ->
    object
        method l: t
        method value: 'a
    end
*)

type ('a) lex_r = <
    l: t;
    ..
> as 'a;;

val create: input: string -> t
(* Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer *)
val next_token: t -> <l: t; tok: Token.t> lex_r
