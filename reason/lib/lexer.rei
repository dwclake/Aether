type t
type lex('a) = {
    ..
    l: t
} as 'a;

let create: (~input:string) => t;
// Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer 
// with the char after that token stored in it
let next_token: t => lex({. l: t, t: Token.t});
