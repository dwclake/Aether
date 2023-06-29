type t = {
    input: string,
    mutable pos: int,
    mutable read_pos: int,
    mutable ch: char
}

let create: (~input:string) => t;
// Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer 
let next_token: ref(t) => Token.t;
