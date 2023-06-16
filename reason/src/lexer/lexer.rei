type t
type lex_r<'a> = {
    ..
    l: t
} as 'a;

let create: (~input:string) => t;
// Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer 
let next_token: t => lex_r<{. l: t, t: Token.t}>;
