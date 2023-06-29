type t

/*
type t1 =
    { .
        input: string,

        pos: int,
        set_pos: int => unit,

        read_pos: int,
        set_read_pos: int => unit,

        ch: char,
        set_ch: char => unit,
};
*/

let create: (~input:string) => t;
// Lexes from the char currently stored in the lexer, returning the corresponding token and a lexer 
let next_token: ref(t) => Token.t;
