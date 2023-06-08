type t

let create: (~input:string) => t;
// Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer 
// with the char after that token stored in it
let next_token: t => (t, Token.t);
